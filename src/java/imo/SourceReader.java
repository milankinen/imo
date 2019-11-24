package imo;

import clojure.lang.*;

import java.util.*;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class SourceReader {
  /* AST node types */
  private static Keyword PROGRAM = Keyword.intern("program");
  private static Keyword NEWLINE = Keyword.intern("newline");
  private static Keyword NIL = Keyword.intern("nil");
  private static Keyword WHITESPACE = Keyword.intern("space");
  private static Keyword NUMBER = Keyword.intern("number");
  private static Keyword BOOLEAN = Keyword.intern("boolean");
  private static Keyword STRING = Keyword.intern("string");
  private static Keyword CHAR = Keyword.intern("char");
  private static Keyword KEYWORD = Keyword.intern("keyword");
  private static Keyword SYMBOL = Keyword.intern("symbol");
  private static Keyword META = Keyword.intern("meta");
  private static Keyword COMMENT = Keyword.intern("comment");
  private static Keyword QUOTE = Keyword.intern("quote");
  private static Keyword DEREF = Keyword.intern("deref");
  private static Keyword SYNTAX_QUOTE = Keyword.intern("syntax_quote");
  private static Keyword UNQUOTE = Keyword.intern("unquote");
  private static Keyword REGEX = Keyword.intern("regex");
  private static Keyword UNQUOTE_SPLICE = Keyword.intern("unquote_splice");
  private static Keyword VAR_QUOTE = Keyword.intern("var_quote");
  private static Keyword DISCARD = Keyword.intern("discard");
  private static Keyword TAGGED_LITERAL = Keyword.intern("tagged_literal");
  private static Keyword READER_COND = Keyword.intern("reader_cond");
  private static Keyword ANON_FN = Keyword.intern("anon_fn");
  private static Keyword LIST = Keyword.intern("list");
  private static Keyword VECTOR = Keyword.intern("vector");
  private static Keyword MAP = Keyword.intern("map");
  private static Keyword NAMESPACE_MAP = Keyword.intern("ns_map");
  private static Keyword SET = Keyword.intern("set");
  private static Keyword SYMBOLIC_VALUE = Keyword.intern("symbolic_val");

  /* Meta attributes for ast nodes */
  private static Keyword META_LINE = Keyword.intern("line");
  private static Keyword META_COL = Keyword.intern("col");
  private static Keyword META_PRE = Keyword.intern("pre");
  private static Keyword META_POST = Keyword.intern("post");

  private static AstNode __END_NODE__ = new AstNode(-1, -1, Keyword.intern("**end**"), List.of());

  private static Pattern SYM_PAT = Pattern.compile("[:]?([\\D&&[^/]].*/)?(/|[\\D&&[^/]][^/]*)");
  private static Pattern INT_PAT =
      Pattern.compile(
          "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?");
  private static Pattern RATIO_PAT = Pattern.compile("([-+]?[0-9]+)/([0-9]+)");
  private static Pattern FLOAT_PAT = Pattern.compile("([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?");


  public static IPersistentVector readAst(String source) {
    return new SourceReader(source).readProgram().toClojure();
  }

  private interface FormReader {
    AstNode readNext(int line, int col);
  }

  private final FormReader[] macros = new FormReader[256];
  private final FormReader[] dispatchMacros = new FormReader[256];
  private final Stack<Integer> pendingEndChars = new Stack<>();
  private final LinkedList<AstNode> pendingNodes = new LinkedList<>();
  private final String _source;
  private final int _len;
  private int _index = 0;
  private int _line = 1;
  private int _col = 1;
  private int _mark = -1;
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

  private AstNode readProgram() {
    List<Object> forms = new LinkedList<>();
    while (true) {
      AstNode form = readNextForm();
      if (form == null) {
        break;
      }
      forms.add(form);
    }
    AstNode program = new AstNode(1, 1, PROGRAM, forms);
    program.post.addAll(pendingNodes);
    return program;
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
        return __END_NODE__;
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
    List<AstNode> pending = new LinkedList<>(pendingNodes);
    AstNode next;
    int nPendingAfter;
    try {
      pendingNodes.clear();
      next = readNextForm();
      nPendingAfter = pendingNodes.size();
    } finally {
      pendingNodes.addAll(pending);
    }
    // readNextForm should always place pendingNodes to readed node's
    // pre position. If that's not the case, we have a potential bug here
    assert (nPendingAfter == 0);
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
    return createAstCodeNode(line, col, NUMBER, List.of(num));
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
        return createAstCodeNode(line, col, NIL, List.of());
      case "true":
        return createAstCodeNode(line, col, BOOLEAN, List.of(true));
      case "false":
        return createAstCodeNode(line, col, BOOLEAN, List.of(false));
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
            return createAstCodeNode(line, col, KEYWORD, List.of(token));
          } else {
            Symbol sym = Symbol.create(token);
            return createAstCodeNode(line, col, SYMBOL, List.of(sym));
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
    return createAstCodeNode(line, col, STRING, List.of(s));
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
    return createAstCodeNode(line, col, REGEX, List.of(regex));
  }

  private AstNode readCharNode(int line, int col) {
    mark();
    int ch = read1();
    if (ch == -1) {
      throw new ReaderException("EOF while reading character");
    } else {
      _col++;
      String token = readToken();
      if (token.length() == 1) {
        return createAstCodeNode(line, col, CHAR, List.of(token));
      } else if (token.startsWith("u")) {
        char c = (char) readUnicodeChar(token, 1, 4, 16);
        if (c >= '\ud800' && c <= '\udfff') {
          throw new ReaderException("Invalid character constant: \\u" + Integer.toString(c, 16));
        } else {
          return createAstCodeNode(line, col, CHAR, List.of(token));
        }
      } else if (token.startsWith("o")) {
        int len = token.length() - 1;
        if (len > 3) {
          throw new ReaderException("Invalid octal escape sequence length: " + len);
        } else {
          int uc = readUnicodeChar(token, 1, len, 8);
          if (uc > 255) {
            throw new ReaderException("Octal escape sequence must be in range [0, 377].");
          } else {
            return createAstCodeNode(line, col, CHAR, List.of(token));
          }
        }
      } else {
        switch (token) {
          case "newline":
          case "space":
          case "tab":
          case "backspace":
          case "formfeed":
          case "return":
            return createAstCodeNode(line, col, CHAR, List.of(token));
          default:
            throw new ReaderException("Unsupported character: \\" + token);
        }
      }
    }
  }

  private AstNode readArgNode(int line, int col) {
    int ch = read1();
    if (ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch)) {
      unread1();
      Symbol sym = Symbol.create(readMarked());
      return createAstCodeNode(line, col, SYMBOL, List.of(sym));
    } else if (ch == '&') {
      _col++;
      Symbol sym = Symbol.create(readMarked());
      return createAstCodeNode(line, col, SYMBOL, List.of(sym));
    } else if (Character.isDigit(ch)) {
      _col++;
      while (Character.isDigit(read1())) _col++;
      unread1();
      Symbol sym = Symbol.create(readMarked());
      return createAstCodeNode(line, col, SYMBOL, List.of(sym));
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
        pendingNodes.add(new AstNode(line, col, COMMENT, List.of(readMarked().trim())));
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
    pendingNodes.add(new AstNode(line, col, DISCARD, List.of(discarded)));
    return readNextForm();
  }

  private AstNode readMetaNode(int line, int col) {
    AstNode form = readNextNestedForm();
    if (form == null) {
      throw new ReaderException("Unexpected EOF while reading metadata");
    }
    Keyword type = form.type;
    if (!(SYMBOL.equals(type) || MAP.equals(type) || KEYWORD.equals(type) || STRING.equals(type))) {
      throw new ReaderException("Metadata must be Symbol, Keyword, String or Map");
    }
    pendingNodes.add(new AstNode(line, col, META, List.of(form)));
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
      next = new AstNode(line, col + 2, DEREF, List.of(next));
    }
    return createAstCodeNode(line, col, READER_COND, List.of(next));
  }

  private AstNode readQuoteNode(int line, int col) {
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF while reading quote");
    }
    return createAstCodeNode(line, col, QUOTE, List.of(inner));
  }

  private AstNode readSyntaxQuoteNode(int line, int col) {
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF while reading syntax quote");
    }
    return createAstCodeNode(line, col, SYNTAX_QUOTE, List.of(inner));
  }

  private AstNode readDerefNode(int line, int col) {
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF while reading deref");
    }
    return createAstCodeNode(line, col, DEREF, List.of(inner));
  }

  private AstNode readVarQuoteNode(int line, int col) {
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF while reading var quote");
    }
    return createAstCodeNode(line, col, VAR_QUOTE, List.of(inner));
  }

  private AstNode readUnquoteNode(int line, int col) {
    int ch = read1();
    Keyword type = UNQUOTE;
    if (ch == '@') {
      _col++;
      type = UNQUOTE_SPLICE;
    } else {
      unread1();
    }
    AstNode inner = readNextNestedForm();
    if (inner == null) {
      throw new ReaderException("Unexpected EOF after unquote");
    }
    return createAstCodeNode(line, col, type, List.of(inner));
  }

  private AstNode readAnonFnNode(int line, int col) {
    return readCollectionNode(ANON_FN, '(', ')', line, col);
  }

  private AstNode readSetNode(int line, int col) {
    return readCollectionNode(SET, '{', '}', line, col);
  }

  private AstNode readListNode(int line, int col) {
    return readCollectionNode(LIST, '(', ')', line, col);
  }

  private AstNode readVectorNode(int line, int col) {
    return readCollectionNode(VECTOR, '[', ']', line, col);
  }

  private AstNode readMapNode(int line, int col) {
    AstNode astNode = readCollectionNode(MAP, '{', '}', line, col);
    if (astNode.children.size() % 2 != 0) {
      throw new ReaderException("Map literal must contain an even number of forms");
    }
    return astNode;
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
      AstNode symbol = next;
      if (symbol.pre.size() > 0) {
        throw new ReaderException("Namespaced map must specify a namespace");
      }
      AstNode map = readNextNestedForm();
      if (map == null) {
        throw new ReaderException("EOF while reading namespace map literal");
      }
      for (AstNode node : map.pre) {
        // Special case: it seems that Clojure Reader does not accept
        // comments or discards or metas between namespace part and map part
        // (e.g. #::foo #_ignore-me {:bar "lol"} so we must treat them
        // equally and throw an error if forms pre-nodes contain anything else
        if (!(WHITESPACE.equals(node.type) || NEWLINE.equals(node.type))) {
          throw new ReaderException("Namespaced map must specify a map");
        }
      }
      if (!MAP.equals(map.type)) {
        throw new ReaderException("Namespaced map must specify a map");
      }
      String nsName = (auto ? "::" : ":") + symbol.children.get(0).toString();
      AstNode ns = new AstNode(symbol.line, symbol.col, KEYWORD, List.of(nsName));
      return createAstCodeNode(line, col, NAMESPACE_MAP, List.of(ns, map));
    } else if (MAP.equals(next.type)) {
      if (!auto) {
        throw new ReaderException("Namespaced map must specify a namespace");
      }
      // case #::{}
      for (AstNode node : next.pre) {
        if (!(WHITESPACE.equals(node.type) || NEWLINE.equals(node.type))) {
          throw new ReaderException("Namespaced map must specify a map");
        }
      }
      AstNode ns = new AstNode(line, col, KEYWORD, List.of("::"));
      AstNode map = next;
      return createAstCodeNode(line, col, NAMESPACE_MAP, List.of(ns, map));
    } else {
      throw new ReaderException("Namespaced map must specify a map");
    }
  }

  private AstNode readCollectionNode(Keyword type, char startChar, char endChar, int line, int col) {
    List items = new LinkedList();
    pendingEndChars.push((int) endChar);
    try {
      while (true) {
        AstNode form = readNextNestedForm();
        if (form == null) {
          throw new ReaderException("Unmatching paren '" + startChar + "'");
        } else if (form == __END_NODE__) {
          return createAstCodeNode(line, col, type, items);
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
      return createAstCodeNode(line, col, TAGGED_LITERAL, List.of(tag, literal));
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
        return createAstCodeNode(line, col, SYMBOLIC_VALUE, List.of(sym));
      }
      default:
        throw new ReaderException("Invalid token: ##" + value);
    }
  }

  /*
   * Helpers
   */

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

  private AstNode createAstCodeNode(int line, int col, Keyword type, List<Object> children) {
    AstNode node = new AstNode(line, col, type, children);
    node.pre.addAll(pendingNodes);
    pendingNodes.clear();
    prevNode = node;
    return node;
  }

  private void handleNewLine(int line, int col) {
    AstNode newLine = new AstNode(line, col, NEWLINE, List.of());
    if (prevNode != null) {
      prevNode.post.addAll(pendingNodes);
      prevNode.post.add(newLine);
      pendingNodes.clear();
    } else {
      pendingNodes.add(newLine);
    }
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
    pendingNodes.add(new AstNode(line, col, WHITESPACE, List.of(ws)));
  }

  private static class AstNode implements Iterable<Object> {
    public final int line;
    public final int col;
    public final Keyword type;
    public final List<Object> children;
    public final List<AstNode> pre = new LinkedList<>();
    public final List<AstNode> post = new LinkedList<>();

    private AstNode(int line, int col, Keyword type, List<Object> children) {
      this.line = line;
      this.col = col;
      this.type = type;
      this.children = children;
    }

    IPersistentVector toClojure() {
      // This is a bit verbose but at least it is FAST
      Map meta =
          !pre.isEmpty() && !post.isEmpty()
              ? Map.of(
              META_LINE, line,
              META_COL, col,
              META_PRE, preAsClj(),
              META_POST, postAsClj())
              : !pre.isEmpty()
              ? Map.of(
              META_LINE, line,
              META_COL, col,
              META_PRE, preAsClj())
              : !post.isEmpty()
              ? Map.of(
              META_LINE, line,
              META_COL, col,
              META_POST, postAsClj())
              : Map.of(
              META_LINE, line,
              META_COL, col);

      return PersistentVector.create(this)
          .withMeta(PersistentArrayMap.create(meta));
    }

    private IPersistentList preAsClj() {
      return PersistentList.create(pre.stream().map(AstNode::toClojure).collect(Collectors.toUnmodifiableList()));
    }

    private IPersistentList postAsClj() {
      return PersistentList.create(post.stream().map(AstNode::toClojure).collect(Collectors.toUnmodifiableList()));
    }

    @Override
    public Iterator<Object> iterator() {
      return new Iterator<Object>() {
        private boolean typeRead = false;
        private Iterator<Object> childIt = children.iterator();

        @Override
        public boolean hasNext() {
          return !typeRead || childIt.hasNext();
        }

        @Override
        public Object next() {
          if (!typeRead) {
            typeRead = true;
            return type;
          }
          Object n = childIt.next();
          if (n instanceof AstNode) {
            return ((AstNode) n).toClojure();
          } else {
            return n;
          }
        }
      };
    }

    @Override
    public void forEach(Consumer<? super Object> action) {
      throw new RuntimeException("Not implemented");
    }

    @Override
    public Spliterator<Object> spliterator() {
      throw new RuntimeException("Not implemented");
    }
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
}
