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
    let tokKind = toTokenKind(tokdat.kind, text: String(text))
    let leadingTrivia = toTrivia(tokdat.leading_trivia, count: tokdat.leading_trivia_count, contents: contents)
    let trailingTrivia = toTrivia(tokdat.trailing_trivia, count: tokdat.trailing_trivia_count, contents: contents)
    return RawSyntax(kind: tokKind, leadingTrivia: leadingTrivia, trailingTrivia: trailingTrivia, presence: .present)
  }

  private func shouldCacheNode(tokdat: swiftparse_token_data_t) -> Bool {
    // This is adapted from RawSyntaxTokenCache::shouldCacheNode() on the C++ side.

    let textLength = Int(tokdat.text.length)
    let tokKind = toTokenKind(tokdat.kind, text: String())

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
  let kind = toSyntaxKind(c_raw.kind)
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

fileprivate func utf8Slice(contents: String, offset: Int, length: Int) -> Substring {
  if length == 0 {
    return Substring()
  }
  let utf8 = contents.utf8
  let begin = utf8.index(utf8.startIndex, offsetBy: offset)
  let end = utf8.index(begin, offsetBy: length)
  return Substring(utf8[begin..<end])
}

// FIXME: The following 'switch' statements are copy-pasted from the gyb-generated files. Provide access to common API.

fileprivate func toTriviaPiece(_ c_piece: swiftparse_trivia_piece_t, contents: String) -> TriviaPiece {
    let kind = c_piece.kind
    let count = Int(c_piece.count)
    let text = String(utf8Slice(contents: contents, offset: Int(c_piece.text.offset), length: Int(c_piece.text.length)))
    switch kind {
    case 0:
      return .spaces(count)      
    case 1:
      return .tabs(count)      
    case 2:
      return .verticalTabs(count)      
    case 3:
      return .formfeeds(count)      
    case 4:
      return .newlines(count)      
    case 5:
      return .carriageReturns(count)      
    case 6:
      return .carriageReturnLineFeeds(count)      
    case 7:
      return .backticks(count)      
    case 8:
      return .lineComment(text)      
    case 9:
      return .blockComment(text)      
    case 10:
      return .docLineComment(text)      
    case 11:
      return .docBlockComment(text)      
    case 12:
      return .garbageText(text)      
    default:
      fatalError()
    }
}

fileprivate func toTokenKind(_ rawValue: UInt32, text: String) -> TokenKind {
    switch rawValue {
    case 0: return .eof
    case 1:
      return .associatedtypeKeyword
    case 2:
      return .classKeyword
    case 3:
      return .deinitKeyword
    case 4:
      return .enumKeyword
    case 5:
      return .extensionKeyword
    case 6:
      return .funcKeyword
    case 7:
      return .importKeyword
    case 8:
      return .initKeyword
    case 9:
      return .inoutKeyword
    case 10:
      return .letKeyword
    case 11:
      return .operatorKeyword
    case 12:
      return .precedencegroupKeyword
    case 13:
      return .protocolKeyword
    case 14:
      return .structKeyword
    case 15:
      return .subscriptKeyword
    case 16:
      return .typealiasKeyword
    case 17:
      return .varKeyword
    case 18:
      return .fileprivateKeyword
    case 19:
      return .internalKeyword
    case 20:
      return .privateKeyword
    case 21:
      return .publicKeyword
    case 22:
      return .staticKeyword
    case 23:
      return .deferKeyword
    case 24:
      return .ifKeyword
    case 25:
      return .guardKeyword
    case 26:
      return .doKeyword
    case 27:
      return .repeatKeyword
    case 28:
      return .elseKeyword
    case 29:
      return .forKeyword
    case 30:
      return .inKeyword
    case 31:
      return .whileKeyword
    case 32:
      return .returnKeyword
    case 33:
      return .breakKeyword
    case 34:
      return .continueKeyword
    case 35:
      return .fallthroughKeyword
    case 36:
      return .switchKeyword
    case 37:
      return .caseKeyword
    case 38:
      return .defaultKeyword
    case 39:
      return .whereKeyword
    case 40:
      return .catchKeyword
    case 50:
      return .throwKeyword
    case 41:
      return .asKeyword
    case 42:
      return .anyKeyword
    case 43:
      return .falseKeyword
    case 44:
      return .isKeyword
    case 45:
      return .nilKeyword
    case 46:
      return .rethrowsKeyword
    case 47:
      return .superKeyword
    case 48:
      return .selfKeyword
    case 49:
      return .capitalSelfKeyword
    case 51:
      return .trueKeyword
    case 52:
      return .tryKeyword
    case 53:
      return .throwsKeyword
    case 54:
      return .__file__Keyword
    case 55:
      return .__line__Keyword
    case 56:
      return .__column__Keyword
    case 57:
      return .__function__Keyword
    case 58:
      return .__dso_handle__Keyword
    case 59:
      return .wildcardKeyword
    case 88:
      return .leftParen
    case 89:
      return .rightParen
    case 90:
      return .leftBrace
    case 91:
      return .rightBrace
    case 92:
      return .leftSquareBracket
    case 93:
      return .rightSquareBracket
    case 94:
      return .leftAngle
    case 95:
      return .rightAngle
    case 85:
      return .period
    case 87:
      return .prefixPeriod
    case 84:
      return .comma
    case 82:
      return .colon
    case 83:
      return .semicolon
    case 86:
      return .equal
    case 80:
      return .atSign
    case 81:
      return .pound
    case 96:
      return .prefixAmpersand
    case 78:
      return .arrow
    case 79:
      return .backtick
    case 100:
      return .backslash
    case 99:
      return .exclamationMark
    case 97:
      return .postfixQuestionMark
    case 98:
      return .infixQuestionMark
    case 102:
      return .stringQuote
    case 103:
      return .multilineStringQuote
    case 74:
      return .poundKeyPathKeyword
    case 69:
      return .poundLineKeyword
    case 73:
      return .poundSelectorKeyword
    case 68:
      return .poundFileKeyword
    case 70:
      return .poundColumnKeyword
    case 72:
      return .poundFunctionKeyword
    case 71:
      return .poundDsohandleKeyword
    case 117:
      return .poundAssertKeyword
    case 65:
      return .poundSourceLocationKeyword
    case 66:
      return .poundWarningKeyword
    case 67:
      return .poundErrorKeyword
    case 64:
      return .poundIfKeyword
    case 62:
      return .poundElseKeyword
    case 63:
      return .poundElseifKeyword
    case 61:
      return .poundEndifKeyword
    case 60:
      return .poundAvailableKeyword
    case 76:
      return .poundFileLiteralKeyword
    case 77:
      return .poundImageLiteralKeyword
    case 75:
      return .poundColorLiteralKeyword
    case 111:
      return .integerLiteral(text)
    case 112:
      return .floatingLiteral(text)
    case 113:
      return .stringLiteral(text)
    case 115:
      return .unknown(text)
    case 105:
      return .identifier(text)
    case 107:
      return .unspacedBinaryOperator(text)
    case 108:
      return .spacedBinaryOperator(text)
    case 110:
      return .postfixOperator(text)
    case 109:
      return .prefixOperator(text)
    case 106:
      return .dollarIdentifier(text)
    case 114:
      return .contextualKeyword(text)
    case 104:
      return .stringSegment(text)
    case 101:
      return .stringInterpolationAnchor
    case 116:
      return .yield
    default:
      // Default to an unknown token with the passed text if we don't know 
      // its kind.
       return .unknown(text)
    }
}

fileprivate func toSyntaxKind(_ rawValue: UInt32) -> SyntaxKind {
    switch rawValue {
    case 0: 
      return .token
    case 1: 
      return .unknown
    case 2: 
      return .unknownDecl
    case 3: 
      return .typealiasDecl
    case 4: 
      return .associatedtypeDecl
    case 5: 
      return .ifConfigDecl
    case 6: 
      return .poundErrorDecl
    case 7: 
      return .poundWarningDecl
    case 8: 
      return .poundSourceLocation
    case 9: 
      return .classDecl
    case 10: 
      return .structDecl
    case 11: 
      return .protocolDecl
    case 12: 
      return .extensionDecl
    case 13: 
      return .functionDecl
    case 14: 
      return .initializerDecl
    case 15: 
      return .deinitializerDecl
    case 16: 
      return .subscriptDecl
    case 17: 
      return .importDecl
    case 18: 
      return .accessorDecl
    case 19: 
      return .variableDecl
    case 20: 
      return .enumCaseDecl
    case 21: 
      return .enumDecl
    case 22: 
      return .operatorDecl
    case 23: 
      return .precedenceGroupDecl
    case 24: 
      return .unknownExpr
    case 25: 
      return .inOutExpr
    case 26: 
      return .poundColumnExpr
    case 27: 
      return .tryExpr
    case 28: 
      return .identifierExpr
    case 29: 
      return .superRefExpr
    case 30: 
      return .nilLiteralExpr
    case 31: 
      return .discardAssignmentExpr
    case 32: 
      return .assignmentExpr
    case 33: 
      return .sequenceExpr
    case 34: 
      return .poundLineExpr
    case 35: 
      return .poundFileExpr
    case 36: 
      return .poundFunctionExpr
    case 37: 
      return .poundDsohandleExpr
    case 38: 
      return .symbolicReferenceExpr
    case 39: 
      return .prefixOperatorExpr
    case 40: 
      return .binaryOperatorExpr
    case 41: 
      return .arrowExpr
    case 42: 
      return .floatLiteralExpr
    case 43: 
      return .tupleExpr
    case 44: 
      return .arrayExpr
    case 45: 
      return .dictionaryExpr
    case 46: 
      return .implicitMemberExpr
    case 47: 
      return .integerLiteralExpr
    case 48: 
      return .stringLiteralExpr
    case 49: 
      return .booleanLiteralExpr
    case 50: 
      return .ternaryExpr
    case 51: 
      return .memberAccessExpr
    case 52: 
      return .dotSelfExpr
    case 53: 
      return .isExpr
    case 54: 
      return .asExpr
    case 55: 
      return .typeExpr
    case 56: 
      return .closureExpr
    case 57: 
      return .unresolvedPatternExpr
    case 58: 
      return .functionCallExpr
    case 59: 
      return .subscriptExpr
    case 60: 
      return .optionalChainingExpr
    case 61: 
      return .forcedValueExpr
    case 62: 
      return .postfixUnaryExpr
    case 63: 
      return .specializeExpr
    case 64: 
      return .stringInterpolationExpr
    case 65: 
      return .keyPathExpr
    case 66: 
      return .keyPathBaseExpr
    case 67: 
      return .objcKeyPathExpr
    case 68: 
      return .objcSelectorExpr
    case 69: 
      return .editorPlaceholderExpr
    case 70: 
      return .objectLiteralExpr
    case 71: 
      return .unknownStmt
    case 72: 
      return .continueStmt
    case 73: 
      return .whileStmt
    case 74: 
      return .deferStmt
    case 75: 
      return .expressionStmt
    case 76: 
      return .repeatWhileStmt
    case 77: 
      return .guardStmt
    case 78: 
      return .forInStmt
    case 79: 
      return .switchStmt
    case 80: 
      return .doStmt
    case 81: 
      return .returnStmt
    case 224: 
      return .yieldStmt
    case 82: 
      return .fallthroughStmt
    case 83: 
      return .breakStmt
    case 84: 
      return .declarationStmt
    case 85: 
      return .throwStmt
    case 86: 
      return .ifStmt
    case 229: 
      return .poundAssertStmt
    case 87: 
      return .decl
    case 88: 
      return .expr
    case 89: 
      return .stmt
    case 90: 
      return .type
    case 91: 
      return .pattern
    case 92: 
      return .codeBlockItem
    case 93: 
      return .codeBlock
    case 94: 
      return .declNameArgument
    case 95: 
      return .declNameArguments
    case 96: 
      return .functionCallArgument
    case 97: 
      return .tupleElement
    case 98: 
      return .arrayElement
    case 99: 
      return .dictionaryElement
    case 100: 
      return .closureCaptureItem
    case 101: 
      return .closureCaptureSignature
    case 102: 
      return .closureParam
    case 103: 
      return .closureSignature
    case 104: 
      return .stringSegment
    case 105: 
      return .expressionSegment
    case 106: 
      return .objcNamePiece
    case 107: 
      return .typeInitializerClause
    case 108: 
      return .parameterClause
    case 109: 
      return .returnClause
    case 110: 
      return .functionSignature
    case 111: 
      return .ifConfigClause
    case 112: 
      return .poundSourceLocationArgs
    case 113: 
      return .declModifier
    case 114: 
      return .inheritedType
    case 115: 
      return .typeInheritanceClause
    case 116: 
      return .memberDeclBlock
    case 117: 
      return .memberDeclListItem
    case 118: 
      return .sourceFile
    case 119: 
      return .initializerClause
    case 120: 
      return .functionParameter
    case 121: 
      return .accessLevelModifier
    case 122: 
      return .accessPathComponent
    case 123: 
      return .accessorParameter
    case 124: 
      return .accessorBlock
    case 125: 
      return .patternBinding
    case 126: 
      return .enumCaseElement
    case 127: 
      return .operatorPrecedenceAndTypes
    case 128: 
      return .precedenceGroupRelation
    case 129: 
      return .precedenceGroupNameElement
    case 130: 
      return .precedenceGroupAssignment
    case 131: 
      return .precedenceGroupAssociativity
    case 132: 
      return .attribute
    case 133: 
      return .labeledSpecializeEntry
    case 227: 
      return .namedAttributeStringArgument
    case 228: 
      return .declName
    case 134: 
      return .implementsAttributeArguments
    case 135: 
      return .objCSelectorPiece
    case 136: 
      return .whereClause
    case 225: 
      return .yieldList
    case 137: 
      return .conditionElement
    case 138: 
      return .availabilityCondition
    case 139: 
      return .matchingPatternCondition
    case 140: 
      return .optionalBindingCondition
    case 141: 
      return .elseIfContinuation
    case 142: 
      return .elseBlock
    case 143: 
      return .switchCase
    case 144: 
      return .switchDefaultLabel
    case 145: 
      return .caseItem
    case 146: 
      return .switchCaseLabel
    case 147: 
      return .catchClause
    case 148: 
      return .genericWhereClause
    case 149: 
      return .sameTypeRequirement
    case 150: 
      return .genericParameter
    case 151: 
      return .genericParameterClause
    case 152: 
      return .conformanceRequirement
    case 153: 
      return .compositionTypeElement
    case 154: 
      return .tupleTypeElement
    case 155: 
      return .genericArgument
    case 156: 
      return .genericArgumentClause
    case 157: 
      return .typeAnnotation
    case 158: 
      return .tuplePatternElement
    case 159: 
      return .availabilityArgument
    case 160: 
      return .availabilityLabeledArgument
    case 161: 
      return .availabilityVersionRestriction
    case 162: 
      return .versionTuple
    case 163: 
      return .codeBlockItemList
    case 164: 
      return .functionCallArgumentList
    case 165: 
      return .tupleElementList
    case 166: 
      return .arrayElementList
    case 167: 
      return .dictionaryElementList
    case 168: 
      return .stringInterpolationSegments
    case 169: 
      return .declNameArgumentList
    case 170: 
      return .exprList
    case 171: 
      return .closureCaptureItemList
    case 172: 
      return .closureParamList
    case 173: 
      return .objcName
    case 174: 
      return .functionParameterList
    case 175: 
      return .ifConfigClauseList
    case 176: 
      return .inheritedTypeList
    case 177: 
      return .memberDeclList
    case 178: 
      return .modifierList
    case 179: 
      return .accessPath
    case 180: 
      return .accessorList
    case 181: 
      return .patternBindingList
    case 182: 
      return .enumCaseElementList
    case 226: 
      return .identifierList
    case 183: 
      return .precedenceGroupAttributeList
    case 184: 
      return .precedenceGroupNameList
    case 185: 
      return .tokenList
    case 186: 
      return .nonEmptyTokenList
    case 187: 
      return .attributeList
    case 188: 
      return .specializeAttributeSpecList
    case 189: 
      return .objCSelector
    case 190: 
      return .switchCaseList
    case 191: 
      return .catchClauseList
    case 192: 
      return .caseItemList
    case 193: 
      return .conditionElementList
    case 194: 
      return .genericRequirementList
    case 195: 
      return .genericParameterList
    case 196: 
      return .compositionTypeElementList
    case 197: 
      return .tupleTypeElementList
    case 198: 
      return .genericArgumentList
    case 199: 
      return .tuplePatternElementList
    case 200: 
      return .availabilitySpecList
    case 201: 
      return .unknownPattern
    case 202: 
      return .enumCasePattern
    case 203: 
      return .isTypePattern
    case 204: 
      return .optionalPattern
    case 205: 
      return .identifierPattern
    case 206: 
      return .asTypePattern
    case 207: 
      return .tuplePattern
    case 208: 
      return .wildcardPattern
    case 209: 
      return .expressionPattern
    case 210: 
      return .valueBindingPattern
    case 211: 
      return .unknownType
    case 212: 
      return .simpleTypeIdentifier
    case 213: 
      return .memberTypeIdentifier
    case 214: 
      return .classRestrictionType
    case 215: 
      return .arrayType
    case 216: 
      return .dictionaryType
    case 217: 
      return .metatypeType
    case 218: 
      return .optionalType
    case 219: 
      return .implicitlyUnwrappedOptionalType
    case 220: 
      return .compositionType
    case 221: 
      return .tupleType
    case 222: 
      return .functionType
    case 223: 
      return .attributedType
    default:
      return .unknown
    }
}