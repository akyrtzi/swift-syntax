import SwiftSyntaxParser
import Dispatch

public class SyntaxParser {
  private let c_parser: swiftparse_parser_t

  public init() {
    c_parser = swiftparse_parser_create()
  }
  deinit {
    swiftparse_parser_dispose(c_parser)
  }

  public func parse(_ contents: String,
    parseOnly: Bool = false,
    isInUTF8: Bool = false,
    useBumpAlloc: Bool = false) throws -> (SourceFileSyntax?, Double) {
    // Get a native UTF8 string for efficient indexing with UTF8 byte offsets.
    // If the string is backed by an NSString then such indexing will become extremely slow.
    let utf8Contents: String
    if isInUTF8 {
      utf8Contents = contents
    } else {
     utf8Contents = contents.withCString { String(cString: $0) }
    }

    if useBumpAlloc {
      swiftparse_alloc_init()
    }

    let start = DispatchTime.now()
    let rawSyntax = parseRaw(utf8Contents, parseOnly: parseOnly, useBumpAlloc: useBumpAlloc)
    let end = DispatchTime.now()
    let nanoTime = end.uptimeNanoseconds - start.uptimeNanoseconds
    let secTime = Double(nanoTime) / 1_000_000_000

    if parseOnly || rawSyntax == nil {
      return (nil, secTime)
    }

    guard let file = makeSyntax(rawSyntax!) as? SourceFileSyntax else {
      throw ParserError.invalidFile
    }
    return (file, secTime)
  }

  func parseRaw(_ contents: String, parseOnly: Bool, useBumpAlloc: Bool) -> RawSyntax? {
    let tokenCache = RawSyntaxTokenCache(contents: contents)
    let nodeHandler: (Optional<UnsafePointer<swiftparse_raw_syntax_node_t>>) -> Optional<UnsafeMutableRawPointer>
    if !parseOnly {
      nodeHandler = { (c_raw_nodeOpt: Optional<UnsafePointer<swiftparse_raw_syntax_node_t>>) -> Optional<UnsafeMutableRawPointer> in
        if useBumpAlloc {
          return UnsafeMutableRawPointer(swiftparse_copy_node(c_raw_nodeOpt))
        }

        let node = makeRawNode(c_raw_nodeOpt!, cache: tokenCache)
        let nodeptr = UnsafeMutablePointer<RawSyntax>.allocate(capacity: 1)
        nodeptr.initialize(to: node)
        return UnsafeMutableRawPointer(nodeptr)
      }
    } else {
      nodeHandler = { _ in return nil }
    }
    swiftparse_set_new_node_handler(c_parser, nodeHandler);

    let c_top = swiftparse_parse(c_parser, contents)
    if (parseOnly || useBumpAlloc) {
      return nil
    }
    return moveFromCRawNode(c_top)
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

  func getToken(_ tokdat: swiftparse_token_data_t, useCache: Bool = true) -> RawSyntax {
    let text = utf8Slice(contents: contents, offset: Int(tokdat.text.offset), length: Int(tokdat.text.length))
    if !useCache || !shouldCacheNode(tokdat: tokdat) {
      return createToken(tokdat, text: text)
    }

    let dataBytes = getDataBytes(tokdat, text: text)
    if let existingNode = cache[dataBytes] {
      return existingNode
    }

    let newNode = createToken(tokdat, text: text)
    cache[dataBytes] = newNode
    return newNode
  }

  private func getDataBytes(_ tokdat: swiftparse_token_data_t, text: Substring) -> [UInt8] {
    var bytes = [UInt8]()
    bytes.reserveCapacity(4 + text.count + (tokdat.leading_trivia_count+tokdat.trailing_trivia_count)*(4+4))
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
    let addTrivia = { (c_ptr: UnsafePointer<swiftparse_trivia_piece_t>?, count: Int) in
      for i in 0..<count {
        let c_piece = c_ptr![i]
        addValue(c_piece.kind)
        addValue(c_piece.count)
        // Trivia with text are not cached.
      }
    }

    addValue(tokdat.kind)
    addString(text)
    addTrivia(tokdat.leading_trivia, tokdat.leading_trivia_count)
    addTrivia(tokdat.trailing_trivia, tokdat.trailing_trivia_count)

    return bytes
  }

  private func createToken(_ tokdat: swiftparse_token_data_t, text: Substring) -> RawSyntax {
    let tokKind = try! TokenKind.create(kind: tokdat.kind, text: String(text))
    let leadingTrivia = toTrivia(tokdat.leading_trivia, count: tokdat.leading_trivia_count, contents: contents)
    let trailingTrivia = toTrivia(tokdat.trailing_trivia, count: tokdat.trailing_trivia_count, contents: contents)
    return RawSyntax(kind: tokKind, leadingTrivia: leadingTrivia, trailingTrivia: trailingTrivia, presence: .present)
  }

  private func shouldCacheNode(tokdat: swiftparse_token_data_t) -> Bool {
    // This is adapted from RawSyntaxTokenCache::shouldCacheNode() on the C++ side.

    let textLength = Int(tokdat.text.length)
    let tokKind = try! TokenKind.create(kind: tokdat.kind, text: String())

    // Is string_literal with >16 length.
    if case .stringLiteral(_) = tokKind, textLength > 16 {
      return false
    }
  
    // Has leading comment trivia et al.
    for i in 0..<tokdat.leading_trivia_count {
      if tokdat.leading_trivia![i].text.length > 0 {
        return false
      }
    }
  
    // Has trailing comment trivia et al.
    for i in 0..<tokdat.trailing_trivia_count {
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
    return cache.getToken(tokdat, useCache: false)
  } else {
    var layout = [RawSyntax?]()
    layout.reserveCapacity(c_raw.layout_data.nodes_count)
    for i in 0..<c_raw.layout_data.nodes_count {
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
  let text = String(utf8Slice(contents: contents, offset: Int(c_piece.text.offset), length: Int(c_piece.text.length)))
  return try! TriviaPiece.create(kind: kind, count: count, text: text)
}

fileprivate func utf8Slice(contents: String, offset: Int, length: Int) -> Substring {
  if length == 0 {
    return Substring()
  }
  let utf8 = contents.utf8
  let begin = utf8.index(utf8.startIndex, offsetBy: offset)
  let end = utf8.index(begin, offsetBy: length)
  return Substring(utf8[begin..<end])
}
