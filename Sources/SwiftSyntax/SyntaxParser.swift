import SwiftSyntaxParser
import Dispatch

public class SyntaxParser {
  private let c_parser: swiftparse_parser_t
  private let synCtx: SyntaxContext

  public init() {
    c_parser = swiftparse_parser_create()
    synCtx = SyntaxContext()
  }
  deinit {
    swiftparse_parser_dispose(c_parser)
  }

  public func parse(_ contents: String,
    parseOnly: Bool = false,
    isInUTF8: Bool = false,
    useBumpAlloc: Bool = false) throws -> (SourceFileSyntax?, CSourceFileSyntax?, Double) {
    // Get a native UTF8 string for efficient indexing with UTF8 byte offsets.
    // If the string is backed by an NSString then such indexing will become extremely slow.
    let utf8Contents: String
    if isInUTF8 {
      utf8Contents = contents
    } else {
     utf8Contents = contents.withCString { String(cString: $0) }
    }

    let start = DispatchTime.now()
    let (rawSyntax, crawNode) = parseRaw(utf8Contents, parseOnly: parseOnly, useBumpAlloc: useBumpAlloc)
    let end = DispatchTime.now()
    let nanoTime = end.uptimeNanoseconds - start.uptimeNanoseconds
    let secTime = Double(nanoTime) / 1_000_000_000

    if parseOnly {
      return (nil, nil, secTime)
    }

    if rawSyntax != nil {
      guard let file = makeSyntax(rawSyntax!) as? SourceFileSyntax else {
        throw ParserError.invalidFile
      }
      return (file, nil, secTime)
    }
    if crawNode != nil {
      guard let file = makeCSyntax(data: crawNode!, ctx: synCtx) as? CSourceFileSyntax else {
        throw ParserError.invalidFile
      }
      return (nil, file, secTime)
    }
    fatalError()
  }

  func parseRaw(_ contents: String, parseOnly: Bool, useBumpAlloc: Bool) -> (RawSyntax?, CRawNode?) {
    let tokenCache = RawSyntaxTokenCache(contents: contents)
    let nodeHandler: (Optional<UnsafePointer<swiftparse_raw_syntax_node_t>>) -> Optional<UnsafeMutableRawPointer>
    if parseOnly {
      nodeHandler = { _ in return nil }
    } else if useBumpAlloc {
      let ctx = self.synCtx // pass it via local variable to avoid unnecessary retain/releases.
      nodeHandler = { (c_raw_nodeOpt: Optional<UnsafePointer<swiftparse_raw_syntax_node_t>>) -> Optional<UnsafeMutableRawPointer> in
        return UnsafeMutableRawPointer(ctx.copyNode(c_raw_nodeOpt!))
      }
    } else {
      nodeHandler = { (c_raw_nodeOpt: Optional<UnsafePointer<swiftparse_raw_syntax_node_t>>) -> Optional<UnsafeMutableRawPointer> in
        let node = makeRawNode(c_raw_nodeOpt!, cache: tokenCache)
        let nodeptr = UnsafeMutablePointer<RawSyntax>.allocate(capacity: 1)
        nodeptr.initialize(to: node)
        return UnsafeMutableRawPointer(nodeptr)
      }
    }
    swiftparse_set_new_node_handler(c_parser, nodeHandler);

    let c_top = swiftparse_parse(c_parser, contents)
    if parseOnly {
      return (nil, nil)
    }
    if useBumpAlloc {
      let c_node = c_top!.bindMemory(to: swiftparse_raw_syntax_node_t.self, capacity: 1)
      return (nil, c_node)
    }
    return (moveFromCRawNode(c_top), nil)
  }
}

struct RawTokenData: Hashable {
  let kind: UInt32
  let text: Substring
}

class RawSyntaxTokenCache {
  private let contents: String
  private var cache = Dictionary<[UInt8], RawSyntax>()

  init(contents: String) {
    self.contents = contents
  }

  func getToken(_ tokdat: swiftparse_token_data_t, kind: swiftparse_token_kind_t, useCache: Bool = true) -> RawSyntax {
    let text = contents.utf8Slice(offset: Int(tokdat.text.offset), length: Int(tokdat.text.length))
    if !useCache || !shouldCacheNode(tokdat: tokdat, kind: kind) {
      return createToken(tokdat, kind: kind, text: text)
    }

    let dataBytes = getDataBytes(tokdat, kind: kind, text: text)
    if let existingNode = cache[dataBytes] {
      return existingNode
    }

    let newNode = createToken(tokdat, kind: kind, text: text)
    cache[dataBytes] = newNode
    return newNode
  }

  private func getDataBytes(_ tokdat: swiftparse_token_data_t, kind: swiftparse_token_kind_t, text: Substring) -> [UInt8] {
    var bytes = [UInt8]()
    bytes.reserveCapacity(4 + Int(text.count) + Int(tokdat.leading_trivia_count+tokdat.trailing_trivia_count)*(4+4))
    let addValue8 = { (val: UInt8) in
      bytes.append(val)
    }
    let addValue = { (val: UInt32) in
      bytes.append(UInt8(truncatingIfNeeded: val))
      bytes.append(UInt8(truncatingIfNeeded: val >> 8))
      bytes.append(UInt8(truncatingIfNeeded: val >> 16))
      bytes.append(UInt8(truncatingIfNeeded: val >> 24))
    }
    let addString = { (str: Substring) in
      if !str.isEmpty {
        bytes += text.utf8
      }
    }
    let addTrivia = { (c_ptr: UnsafePointer<swiftparse_trivia_piece_t>?, count: UInt32) in
      for i in 0..<Int(count) {
        let c_piece = c_ptr![i]
        addValue8(c_piece.kind)
        addValue(c_piece.count)
        // Trivia with text are not cached.
      }
    }

    addValue8(kind)
    addString(text)
    addTrivia(tokdat.leading_trivia, tokdat.leading_trivia_count)
    addTrivia(tokdat.trailing_trivia, tokdat.trailing_trivia_count)

    return bytes
  }

  private func createToken(_ tokdat: swiftparse_token_data_t, kind: swiftparse_token_kind_t, text: Substring) -> RawSyntax {
    let tokKind = try! TokenKind.create(kind: kind, text: String(text))
    let leadingTrivia = toTrivia(tokdat.leading_trivia, count: Int(tokdat.leading_trivia_count), contents: contents)
    let trailingTrivia = toTrivia(tokdat.trailing_trivia, count: Int(tokdat.trailing_trivia_count), contents: contents)
    return RawSyntax(kind: tokKind, leadingTrivia: leadingTrivia, trailingTrivia: trailingTrivia, presence: .present)
  }

  private func shouldCacheNode(tokdat: swiftparse_token_data_t, kind: swiftparse_token_kind_t) -> Bool {
    // This is adapted from RawSyntaxTokenCache::shouldCacheNode() on the C++ side.

    let textLength = Int(tokdat.text.length)
    let tokKind = try! TokenKind.create(kind: kind, text: String())

    // Is string_literal with >16 length.
    if case .stringLiteral(_) = tokKind, textLength > 16 {
      return false
    }
  
    // Has leading comment trivia et al.
    for i in 0..<Int(tokdat.leading_trivia_count) {
      if tokdat.leading_trivia![i].text.length > 0 {
        return false
      }
    }
  
    // Has trailing comment trivia et al.
    for i in 0..<Int(tokdat.trailing_trivia_count) {
      if tokdat.trailing_trivia![i].text.length > 0 {
        return false
      }
    }
  
    // We can cache the node
    return true;
  }
}

fileprivate func makeRawNode(_ c_raw_ptr: UnsafePointer<swiftparse_raw_syntax_node_t>, cache: RawSyntaxTokenCache) -> RawSyntax {
  let c_raw = c_raw_ptr.pointee
  let kind = SyntaxKind.fromRawValue(c_raw.kind)
  if kind == .token {
    let tokdat = c_raw.token_data
    // Using the cache slows down performance.
    return cache.getToken(tokdat, kind: c_raw.token_kind, useCache: false)
  } else {
    var layout = [RawSyntax?]()
    layout.reserveCapacity(Int(c_raw.layout_data.nodes_count))
    for i in 0..<Int(c_raw.layout_data.nodes_count) {
      let subnode = moveFromCRawNode(c_raw.layout_data.nodes![i])
      layout.append(subnode)
    }
    return RawSyntax(kind: kind, layout: layout, presence: .present)
  }
}

fileprivate func moveFromCRawNode(_ ptr: Optional<UnsafeMutableRawPointer>) -> RawSyntax? { 
  if ptr == nil {
    return nil
  }
  let c_node = ptr!.bindMemory(to: RawSyntax.self, capacity: 1)
  let node = c_node.pointee
  c_node.deinitialize(count:1)
  c_node.deallocate()
  return node
}

fileprivate func toTrivia(_ c_ptr: UnsafePointer<swiftparse_trivia_piece_t>?, count: Int, contents: String) -> Trivia {
  var pieces = [TriviaPiece]()
  pieces.reserveCapacity(count)
  for i in 0..<count {
    let piece = toTriviaPiece(c_ptr![i], contents: contents)
    pieces.append(piece)
  }
  return Trivia(pieces: pieces)
}

fileprivate func toTriviaPiece(_ c_piece: swiftparse_trivia_piece_t, contents: String) -> TriviaPiece {
  let kind = c_piece.kind
  let count = Int(c_piece.count)
  let text = String(contents.utf8Slice(offset: Int(c_piece.text.offset), length: Int(c_piece.text.length)))
  return try! TriviaPiece.create(kind: kind, count: count, text: text)
}
