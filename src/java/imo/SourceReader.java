package imo;

import clojure.lang.Keyword;
import clojure.lang.PersistentVector;

import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static imo.AstNode.*;
import static imo.Keywords.*;

public class SourceReader {
  private static Pattern SYM_PAT = Pattern.compile("[:]?([\\D&&[^/]].*/)?(/|[\\D&&[^/]][^/]*)");
  private static Pattern INT_PAT =
      Pattern.compile(
          "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?");
  private static Pattern RATIO_PAT = Pattern.compile("([-+]?[0-9]+)/([0-9]+)");
  private static Pattern FLOAT_PAT = Pattern.compile("([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?");
  private static Keyword END_OF_COLL = Keyword.intern("*end-of-coll*");

  public static PersistentVector readAst(String source) {
    source = source.replaceAll("(\n|\r\n|\r)", "\n");
    return new SourceReader(source).readRoot().toVec();
  }

  private interface FormReader {
    AstNode readNext(int line, int col);
  }

  private final FormReader[] macros = new FormReader[256];
  private final FormReader[] dispatchMacros = new FormReader[256];
  private final Stack<Integer> pendingEndChars = new Stack<>();
  private final String _source;
  private final int _len;
  private int _index = 0;
  private int _line = 1;
  private int _col = 1;
  private int _mark = -1;
  private LinkedList<AstNode> pendingNodes = null;
  private AstNode prevNode = null;

  private SourceReader(String source) {
    _source = source;
    _len = _source.length();
    macros['"'] = this::readStringNode;
    macros[';'] = this::readCommentNode;
    macros['\''] = this::readQuoteNode;
    macros['@'] = this::readDerefNode;
    macros['^'] = this::readMetaNode;
    macros['`'] = this::readSyntaxQuoteNode;
    macros['~'] = this::readUnquoteNode;
    macros['('] = this::readListNode;
    macros[')'] = this::throwUnmatchedDelimiter;
    macros['['] = this::readVectorNode;
    macros[']'] = this::throwUnmatchedDelimiter;
    macros['{'] = this::readMapNode;
    macros['}'] = this::throwUnmatchedDelimiter;
    macros['\\'] = this::readCharNode;
    macros['%'] = this::readArgNode;
    macros['#'] = this::readDispatchNode;
    dispatchMacros['^'] = this::readMetaNode;
    dispatchMacros['#'] = this::readSymbolicValueNode;
    dispatchMacros['\''] = this::readVarQuoteNode;
    dispatchMacros['"'] = this::readRegexNode;
    dispatchMacros['('] = this::readAnonFnNode;
    dispatchMacros['{'] = this::readSetNode;
    dispatchMacros['='] = this::readEvalNode;
    dispatchMacros['!'] = this::readCommentNode;
    //dispatchMacros['<'] = new UnreadableReader();
    dispatchMacros['_'] = this::readDiscardNode;
    dispatchMacros['?'] = this::readReaderConditionalNode;
    dispatchMacros[':'] = this::readNsMapNode;
  }

  private AstNode readRoot() {
    LinkedList<AstNode> forms = new LinkedList<>();
    while (true) {
      AstNode form = readNextForm();
      if (form == null) {
        break;
      }
      forms.add(form);
    }
    AstNode root = createRoot(forms);
    root.post = pendingNodes;
    return root;
  }

  private AstNode readNextForm() {
    prevNode = null;
    for (; ; ) {
      mark();
      int line = _line;
      int col = _col;
      int ch = read1();
      if (ch == -1) {
        return null;
      }
      if (isNewline(ch)) {
        _line++;
        _col = 1;
        handleNewLine(line, col);
        continue;
      } else {
        _col++;
      }
      if (isWhitespace(ch)) {
        handleWhitespace(line, col);
        continue;
      }
      if (Character.isDigit(ch)) {
        return readNumberNode(line, col);
      }

      if (!pendingEndChars.empty() && pendingEndChars.peek().equals(ch)) {
        return endOfColl(line, col);
      }

      FormReader macroReader = getMacro(ch);
      if (macroReader != null) {
        return macroReader.readNext(line, col);
      }
      if (ch == '+' || ch == '-') {
        int ch2 = read1();
        if (Character.isDigit(ch2)) {
          unread1();
          return readNumberNode(line, col);
        } else {
          unread1();
        }
      }
      String token = readToken();
      return tokenToAstNode(token, line, col);
    }
  }

  private AstNode readNextNestedForm() {
    // Ensure that discarded form gets it's own pre and post
    // nodes, e.g. case:  #_ ^:foo bar "tsers"
    // bar should have ^:foo as pre and the discard node
    // should be pre (or post) of the outer level
    LinkedList<AstNode> pending = pendingNodes;
    LinkedList<AstNode> after = null;
    AstNode next;
    try {
      pendingNodes = null;
      next = readNextForm();
      after = pendingNodes;
    } finally {
      pendingNodes = pending;
    }
    // readNextForm should always place pendingNodes to readed node's
    // pre position. If that's not the case, we have a potential bug here
    assert after == null || after.isEmpty();
    return next;
  }

  /*
   * Leaf nodes: these nodes do not contain any other nested
   * nodes, thus they do not attempt to use readNextForm recursively
   */

  private AstNode readNumberNode(int line, int col) {
    for (; ; ) {
      int ch = read1();
      if (ch == -1 || isWhitespace(ch) || isMacro(ch)) {
        unread1();
        break;
      } else {
        _col++;
      }
    }

    String num = readMarked();
    if (!isNumber(num)) {
      throw new NumberFormatException("Invalid number: " + num);
    }
    return withPre(createNumber(line, col, num));
  }

  private String readToken() {
    for (; ; ) {
      int ch = read1();
      if (ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch)) {
        unread1();
        return readMarked();
      } else {
        _col++;
      }
    }
  }

  private AstNode tokenToAstNode(String token, int line, int col) {
    switch (token) {
      case "nil":
        return withPre(createNil(line, col));
      case "true":
        return withPre(createBoolean(line, col, true));
      case "false":
        return withPre(createBoolean(line, col, false));
      default: {
        Matcher m = SYM_PAT.matcher(token);
        if (m.matches()) {
          String ns = m.group(1);
          String name = m.group(2);
          if (ns != null && ns.endsWith(":/")
              || name.endsWith(":")
              || token.indexOf("::", 1) != -1) {
            throw new ReaderException("Invalid symbol: " + token);
          }
          if (token.charAt(0) == ':') {
            return withPre(createKeyword(line, col, token));
          } else {
            return withPre(createSymbol(line, col, token));
          }
        }
        throw new ReaderException("Invalid token: " + token);
      }
    }
  }

  private AstNode readStringNode(int line, int col) {
    for (int ch = read1(); ch != '"'; ch = read1(), _col++) {
      if (ch == -1) {
        throw new ReaderException("EOF while reading string");
      }

      if (isNewline(ch)) {
        _line++;
        _col = 1;
        continue;
      }

      if (ch == '\\') {
        //escaped character
        ch = read1();
        _col++;
        if (ch == -1) {
          throw new ReaderException("EOF while reading string");
        }
        switch (ch) {
          case 't':
          case 'r':
          case 'n':
          case '\\':
          case '"':
          case 'b':
          case 'f':
            break;
          case 'u': {
            ch = read1();
            if (Character.digit(ch, 16) == -1) {
              throw new ReaderException("Invalid unicode escape: \\u" + (char) ch);
            }
            readUnicodeChar(ch, 16, 4, true);
            break;
          }
          default: {
            if (Character.isDigit(ch)) {
              ch = readUnicodeChar(ch, 8, 3, false);
              if (ch > 0377) {
                throw new ReaderException("Octal escape sequence must be in range [0, 377].");
              }
            } else {
              throw new ReaderException("Unsupported escape character: \\" + (char) ch);
            }
          }
        }
      }
    }
    String s = readMarked();
    return withPre(createString(line, col, s));
  }

  private AstNode readRegexNode(int line, int col) {
    for (int ch = read1(); ch != '"'; ch = read1(), _col++) {
      if (ch == -1) {
        throw new ReaderException("EOF while reading regex");
      }
      if (isNewline(ch)) {
        _line++;
        _col = 1;
      }
      if (ch == '\\') {
        // escape
        ch = read1();
        if (ch == -1)
          throw new ReaderException("EOF while reading regex");
      }
    }
    String regex = readMarked();
    try {
      Pattern.compile(regex);
    } catch (Exception e) {
      throw new ReaderException("Invalid regex");
    }
    return withPre(createRegex(line, col, regex));
  }

  private AstNode readCharNode(int line, int col) {
    int ch = read1();
    if (ch == -1) {
      throw new ReaderException("EOF while reading character");
    } else {
      _col++;
      String token = readToken();
      String chStr = token.substring(1);
      if (chStr.length() == 1) {
        return withPre(createChar(line, col, token));
      } else if (chStr.startsWith("u")) {
        char c = (char) readUnicodeChar(chStr, 1, 4, 16);
        if (c >= '\ud800' && c <= '\udfff') {
          throw new ReaderException("Invalid character constant: \\u" + Integer.toString(c, 16));
        } else {
          return withPre(createChar(line, col, token));
        }
      } else if (chStr.startsWith("o")) {
        int len = chStr.length() - 1;
        if (len > 3) {
          throw new ReaderException("Invalid octal escape sequence length: " + len);
        } else {
          int uc = readUnicodeChar(chStr, 1, len, 8);
          if (uc > 255) {
            throw new ReaderException("Octal escape sequence must be in range [0, 377].");
          } else {
            return withPre(createChar(line, col, token));
          }
        }
      } else {
        switch (chStr) {
          case "newline":
          case "space":
          case "tab":
          case "backspace":
          case "formfeed":
          case "return":
            return withPre(createChar(line, col, token));
          default:
            throw new ReaderException("Unsupported character: \\" + chStr);
        }
      }
    }
  }

  private AstNode readArgNode(int line, int col) {
    int ch = read1();
    if (ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch)) {
      unread1();
      return withPre(createSymbol(line, col, readMarked()));
    } else if (ch == '&') {
      _col++;
      return withPre(createSymbol(line, col, readMarked()));
    } else if (Character.isDigit(ch)) {
      _col++;
      while (Character.isDigit(read1())) _col++;
      unread1();
      return withPre(createSymbol(line, col, readMarked()));
    } else {
      throw new ReaderException("Arg literal must be %, %& or %integer");
    }
  }

  /*
   * Non-evaluable code nodes: these nodes do not add new AST node
   * directly to the AST. Instead they will be added to :pre and :post
   * positions.
   *
   * :post nodes are nodes that appear after the actual node BUT before
   * the next newline or EOF.
   *
   * :pre nodes are nodes that appear before the next actual nodes, including
   * newlines but excluding :post nodes from the previous actual node.
   *
   * E.g.
   *
   * ; this is comment
   * ^:foo bar ; lol
   * ; bal
   * tsers
   *
   * Will result to (simplified):
   *
   * ^{:pre [[:comment "; this is comment"]
   *         [:meta ":foo"]]
   *   :post [[:comment "; lol"]]}
   * [:symbol bar]
   *
   * ^{:pre [[:comment "; bal"]]}
   * [:symbol tsers]
   */

  private AstNode readCommentNode(int line, int col) {
    while (true) {
      int ch = read1();
      if (ch == -1 || isNewline(ch)) {
        if (ch == -1) {
          unread1();
        } else {
          _line++;
          _col = 1;
        }
        addPending(createComment(line, col, readMarked()));
        break;
      } else {
        _col++;
      }
    }
    return readNextForm();
  }

  private AstNode readDiscardNode(int line, int col) {
    AstNode discarded = readNextNestedForm();
    if (discarded == null) {
      throw new ReaderException("Unexpected EOF after discard");
    }
    addPending(createDiscard(line, col, discarded));
    return readNextForm();
  }

  private AstNode readMetaNode(int line, int col) {
    AstNode form = readNextNestedForm();
    if (form == null) {
      throw new ReaderException("Unexpected EOF while reading metadata");
    }
    Keyword type = form.type;
    if (!(SYMBOL.equals(type)
        || MAP.equals(type)
        || KEYWORD.equals(type)
        || STRING.equals(type))) {
      throw new ReaderException("Metadata must be Symbol, Keyword, String or Map");
    }
    addPending(createMeta(line, col, form));
    return readNextForm();
  }

  /*
   * Forms that can be evaluated and contains nested forms.
   */

  private AstNode readReaderConditionalNode(int line, int col) {
    int ch = read1();
    boolean splice = false;
    if (ch == '@') {
      splice = true;
      _col++;
    } else {
      unread1();
    }
    AstNode next = readNextNestedForm();
    if (next == null) {
      throw new ReaderException("Unexpected EOF while reading reader conditional");
    }
    if (!LIST.equals(next.type)) {
      throw new ReaderException("Expecting list after reader conditional");
    }
    if (splice) {
      next = createDeref(line, col + 2, next);
    }
    return withPre(createReaderCond(line, col, next));
  }

  private AstNode readQuoteNode(int line, int col) {
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF while reading quote");
    }
    return withPre(createQuote(line, col, inner));
  }

  private AstNode readSyntaxQuoteNode(int line, int col) {
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF while reading syntax quote");
    }
    return withPre(createSyntaxQuote(line, col, inner));
  }

  private AstNode readDerefNode(int line, int col) {
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF while reading deref");
    }
    return withPre(createDeref(line, col, inner));
  }

  private AstNode readVarQuoteNode(int line, int col) {
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF while reading var quote");
    }
    return withPre(createVarQuote(line, col, inner));
  }

  private AstNode readUnquoteNode(int line, int col) {
    int ch = read1();
    boolean splice = false;
    if (ch == '@') {
      _col++;
      splice = true;
    } else {
      unread1();
    }
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF after unquote");
    }
    return withPre(splice
        ? createUnquoteSplice(line, col, inner)
        : createUnquote(line, col, inner));
  }

  private AstNode readAnonFnNode(int line, int col) {
    return readCollectionNode(AstNode::createAnonFn, ')', line, col);
  }

  private AstNode readSetNode(int line, int col) {
    return readCollectionNode(AstNode::createSet, '}', line, col);
  }

  private AstNode readListNode(int line, int col) {
    return readCollectionNode(AstNode::createList, ')', line, col);
  }

  private AstNode readVectorNode(int line, int col) {
    return readCollectionNode(AstNode::createVector, ']', line, col);
  }

  private AstNode readMapNode(int line, int col) {
    AstNode collNode = readCollectionNode(AstNode::createMap, '}', line, col);
    if (collNode.children.size() % 2 != 0) {
      throw new ReaderException("Map literal must contain an even number of forms");
    }
    return collNode;
  }

  private AstNode readNsMapNode(int line, int col) {
    boolean auto = false;
    int autoChar = read1();
    if (autoChar == ':') {
      auto = true;
      _col++;
    } else {
      unread1();
    }

    AstNode next = readNextNestedForm();
    if (next == null) {
      throw new ReaderException("EOF while reading namespace map literal");
    }
    if (SYMBOL.equals(next.type)) {
      AstNode symbolPart = next;
      if (symbolPart.pre != null) {
        throw new ReaderException("Namespaced map must specify a namespace");
      }
      AstNode mapPart = readNextNestedForm();
      if (mapPart == null) {
        throw new ReaderException("EOF while reading namespace map literal");
      }
      if (!MAP.equals(mapPart.type)) {
        throw new ReaderException("Namespaced map must specify a map");
      }
      String nsName = (auto ? "::" : ":") + symbolPart.children.get(0);
      AstNode ns = createKeyword(symbolPart.line, symbolPart.col, nsName);
      return withPre(createNsMap(line, col, List.of(ns, mapPart)));
    } else if (MAP.equals(next.type)) {
      if (!auto) {
        throw new ReaderException("Namespaced map must specify a namespace");
      }
      // case #::{}
      AstNode nsPart = createKeyword(line, col, "::");
      AstNode mapPart = next;
      return withPre(createNsMap(line, col, List.of(nsPart, mapPart)));
    } else {
      throw new ReaderException("Namespaced map must specify a map");
    }
  }

  private interface CollNodeCtor {
    AstNode create(int line, int col, List<AstNode> children);
  }

  private AstNode readCollectionNode(CollNodeCtor ctor, char endChar, int line, int col) {
    LinkedList<AstNode> items = new LinkedList<>();
    pendingEndChars.push((int) endChar);
    try {
      while (true) {
        AstNode form = readNextNestedForm();
        if (form == null) {
          throw new ReaderException("Unmatching paren '" + endChar + "'");
        } else if (form.type == END_OF_COLL) {
          // There might be non-code nodes such as white spaces
          // between the latest code form and __END__. In this case, those
          // nodes are stored as __END__'s pre position so we need to
          // lift them to latest form's post position and if that is not possible
          // we must use collection node's hidden content
          if (!items.isEmpty()) {
            // e.g. (foo )
            AstNode lastForm = items.getLast();
            assert lastForm.post == null;
            lastForm.post = form.pre;
            return withPre(ctor.create(line, col, items));
          } else {
            // e.g. [ ]
            AstNode collNode = withPre(ctor.create(line, col, items));
            assert collNode.hidden == null;
            collNode.hidden = form.pre;
            return collNode;
          }
        } else {
          items.add(form);
        }
      }
    } finally {
      pendingEndChars.pop();
    }
  }

  private AstNode readEvalNode(int line, int col) {
    throw new ReaderException("Eval macro not supported");
  }

  private AstNode readDispatchNode(int line, int col) {
    int ch = read1();
    if (ch == -1) {
      throw new ReaderException("EOF while reading character");
    }
    FormReader reader = ch < dispatchMacros.length ? dispatchMacros[ch] : null;
    if (reader == null) {
      // Try interpret as tagged literal
      unread1();
      AstNode tag = readNextNestedForm();
      if (tag == null) {
        throw new ReaderException("EOF while reading dispatch macro");
      }
      if (!SYMBOL.equals(tag.type)) {
        throw new ReaderException("Reader tag must be a symbol");
      }
      AstNode literal = readNextNestedForm();
      if (literal == null) {
        throw new ReaderException("Missing tagged literal value");
      }
      return withPre(createTaggedLiteral(line, col, List.of(tag, literal)));
    }
    _col++;
    return reader.readNext(line, col);
  }

  private AstNode readSymbolicValueNode(int line, int col) {
    AstNode sym = readNextNestedForm();
    if (sym == null) {
      throw new ReaderException("Unexpected EOF while reading var quote");
    }
    if (!SYMBOL.equals(sym.type)) {
      throw new ReaderException("Unknown symbolic value");
    }
    String value = sym.children.get(0).toString();
    switch (value) {
      case "Inf":
      case "-Inf":
      case "NaN": {
        return withPre(createSymbolicVal(line, col, sym));
      }
      default:
        throw new ReaderException("Invalid token: ##" + value);
    }
  }

  /*
   * Helpers
   */

  private AstNode endOfColl(int line, int col) {
    return withPre(new AstNode(line, col, END_OF_COLL, List.of()));
  }

  private int readUnicodeChar(String token, int offset, int length, int base) {
    if (token.length() != offset + length) {
      throw new ReaderException("Invalid unicode character: \\" + token);
    } else {
      int uc = 0;

      for (int i = offset; i < offset + length; ++i) {
        int d = Character.digit(token.charAt(i), base);
        if (d == -1) {
          throw new ReaderException("Invalid digit: " + token.charAt(i));
        }

        uc = uc * base + d;
      }

      return (char) uc;
    }
  }

  private int readUnicodeChar(int initch, int base, int length, boolean exact) {
    int uc = Character.digit(initch, base);
    if (uc == -1) {
      throw new ReaderException("Invalid digit: " + (char) initch);
    }
    int i = 1;
    for (; i < length; ++i) {
      int ch = read1();
      if (ch == -1 || isWhitespace(ch) || isMacro(ch)) {
        unread1();
        break;
      }
      int d = Character.digit(ch, base);
      if (d == -1) {
        throw new ReaderException("Invalid digit: " + (char) ch);
      }
      uc = uc * base + d;
    }
    if (i != length && exact) {
      throw new ReaderException("Invalid character length: " + i + ", should be: " + length);
    }
    return uc;
  }

  private AstNode throwUnmatchedDelimiter(int line, int col) {
    unread1();
    char delim = (char) read1();
    throw new ReaderException("Unmatched delimiter: " + delim);
  }

  private boolean isMacro(int ch) {
    return ch >= 0 && ch < macros.length && macros[ch] != null;
  }

  private boolean isTerminatingMacro(int ch) {
    return (ch != '#' && ch != '\'' && ch != '%' && isMacro(ch));
  }

  private FormReader getMacro(int ch) {
    return isMacro(ch) ? macros[ch] : null;
  }

  private void mark() {
    _mark = _index;
  }

  private String readMarked() {
    return _source.substring(_mark, _index);
  }

  private boolean isEOF() {
    return _index >= _len;
  }

  private int read1() {
    if (isEOF()) {
      _index++;
      return -1;
    }
    return _source.charAt(_index++);
  }

  private void unread1() {
    _index--;
  }

  private void handleNewLine(int line, int col) {
    addPending(createNewline(line, col));
    if (prevNode != null) {
      assert prevNode.post == null;
      prevNode.post = pendingNodes;
      pendingNodes = null;
      prevNode = null;
    }
  }

  private <NodeType extends AstNode> NodeType withPre(NodeType codeNode) {
    assert codeNode.pre == null;
    codeNode.pre = pendingNodes;
    pendingNodes = null;
    return codeNode;
  }

  private void handleWhitespace(int line, int col) {
    while (true) {
      int c = read1();
      if (!isWhitespace(c) || isNewline(c)) {
        break;
      } else {
        _col++;
      }
    }
    ;
    unread1();
    String ws = readMarked();
    addPending(createSpace(line, col, ws));
  }

  private void addPending(AstNode pending) {
    if (pendingNodes == null) {
      pendingNodes = new LinkedList<>();
    }
    pendingNodes.add(pending);
  }

  private static boolean isNewline(int ch) {
    return ch == '\n';
  }

  private static boolean isWhitespace(int ch) {
    return (Character.isWhitespace(ch) || ch == ',');
  }

  private static boolean isNumber(String s) {
    Matcher m = INT_PAT.matcher(s);
    if (m.matches()) {
      if (m.group(2) != null) {
        if (m.group(8) != null)
          return true;
        return true;
      }
      if (m.group(3) != null)
        return true;
      else if (m.group(4) != null)
        return true;
      else if (m.group(5) != null)
        return true;
      else if (m.group(7) != null)
        return true;
      return false;
    }
    m = FLOAT_PAT.matcher(s);
    if (m.matches()) {
      return true;
    }
    m = RATIO_PAT.matcher(s);
    return m.matches();
  }

  public static class ReaderException extends ImoException {
    public ReaderException(String message) {
      super(message);
    }
  }
}
