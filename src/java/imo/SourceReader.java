package imo;

import clojure.lang.IPersistentVector;
import clojure.lang.Keyword;
import clojure.lang.PersistentVector;

import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SourceReader {
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
  private static Keyword REGEXP = Keyword.intern("regexp");
  private static Keyword UNQUOTE_SPLICE = Keyword.intern("unquote_splice");
  private static Keyword VAR_QUOTE = Keyword.intern("var_quote");
  private static Keyword IGNORE = Keyword.intern("ignore");
  private static Keyword TAGGED_LITERAL = Keyword.intern("tagged_literal");
  private static Keyword READER_COND = Keyword.intern("reader_cond");
  private static Keyword ANON_FN = Keyword.intern("anon_fn");
  private static Keyword LIST = Keyword.intern("list");
  private static Keyword VECTOR = Keyword.intern("vector");
  private static Keyword MAP = Keyword.intern("map");
  private static Keyword NAMESPACE_MAP = Keyword.intern("ns_map");
  private static Keyword SET = Keyword.intern("set");
  private static Keyword SYMBOLIC_VALUE = Keyword.intern("symbolic_val");

  private static PersistentVector NEWLINE_NODE = astNode(List.of(NEWLINE));
  private static PersistentVector NIL_NODE = astNode(List.of(NIL));
  private static PersistentVector END_NODE = astNode(List.of(Keyword.intern("*end*")));

  private static Pattern SYM_PAT = Pattern.compile("[:]?([\\D&&[^/]].*/)?(/|[\\D&&[^/]][^/]*)");
  private static Pattern INT_PAT =
      Pattern.compile(
          "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?");
  private static Pattern RATIO_PAT = Pattern.compile("([-+]?[0-9]+)/([0-9]+)");
  private static Pattern FLOAT_PAT = Pattern.compile("([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?");


  public static IPersistentVector readAst(String source) {
    return new SourceReader(source).readProgram();
  }

  public static class ReaderException extends RuntimeException {
    ReaderException(String message) {
      super(message);
    }
  }

  private interface FormReader {
    PersistentVector readNext();
  }

  private final FormReader[] macros = new FormReader[256];
  private final FormReader[] dispatchMacros = new FormReader[256];
  private final Stack<Integer> pendingEndChars = new Stack<>();
  private final String source;
  private final int len;
  private int index = 0;
  private int line = 1;
  private int col = 0;
  private int _mark = -1;

  private SourceReader(String source) {
    this.source = source;
    this.len = this.source.length();
    this.macros['"'] = this::readString;
    this.macros[';'] = this::readComment;
    this.macros['\''] = this::readQuote;
    this.macros['@'] = this::readDeref;
    this.macros['^'] = this::readMeta;
    this.macros['`'] = this::readSyntaxQuote;
    this.macros['~'] = this::readUnquote;
    this.macros['('] = this::readList;
    this.macros[')'] = this::throwUnmatchedDelimiter;
    this.macros['['] = this::readVector;
    this.macros[']'] = this::throwUnmatchedDelimiter;
    this.macros['{'] = this::readMap;
    this.macros['}'] = this::throwUnmatchedDelimiter;
    this.macros['\\'] = this::readChar;
    this.macros['%'] = this::readArg;
    this.macros['#'] = this::readDispatch;
    this.dispatchMacros['^'] = this::readMeta;
    this.dispatchMacros['#'] = this::readSymbolicValue;
    this.dispatchMacros['\''] = this::readVar;
    this.dispatchMacros['"'] = this::readRegexp;
    this.dispatchMacros['('] = this::readFn;
    this.dispatchMacros['{'] = this::readSet;
    this.dispatchMacros['='] = this::readEval;
    this.dispatchMacros['!'] = this::readComment;
    //this.dispatchMacros['<'] = new UnreadableReader();
    this.dispatchMacros['_'] = this::readIgnore;
    this.dispatchMacros['?'] = this::readReaderConditional;
    this.dispatchMacros[':'] = this::readNsMap;
  }

  PersistentVector readProgram() {
    LinkedList<Object> forms = new LinkedList<>();
    forms.add(PROGRAM);
    while (true) {
      Object next = readNext();
      if (next == null) {
        break;
      }
      forms.add(next);
    }
    return astNode(forms);
  }

  private PersistentVector readNext() {
    mark();
    int ch = read1();
    if (ch == -1) {
      return null;
    }
    if (isNewline(ch)) {
      return NEWLINE_NODE;
    }
    if (isWhitespace(ch)) {
      while (true) {
        int c = read1();
        if (!isWhitespace(c) || isNewline(c)) {
          break;
        }
      }
      ;
      unread1();
      return astNode(List.of(WHITESPACE, readMarked()));
    }
    if (Character.isDigit(ch)) {
      return readNumber();
    }

    if (!pendingEndChars.empty() && pendingEndChars.peek().equals(ch)) {
      return END_NODE;
    }

    FormReader macroReader = getMacro(ch);
    if (macroReader != null) {
      return macroReader.readNext();
    }
    if (ch == '+' || ch == '-') {
      int ch2 = read1();
      if (Character.isDigit(ch2)) {
        unread1();
        return readNumber();
      } else {
        unread1();
      }
    }
    String token = readToken();
    return tokenToAstNode(token);
  }

  private LinkedList<PersistentVector> readNextIgnoreWhitespace() {
    LinkedList<PersistentVector> forms = new LinkedList<>();
    while (true) {
      PersistentVector next = readNext();
      if (next != null) {
        forms.add(next);
        Object type = next.seq().first();
        if (WHITESPACE.equals(type)
            || NEWLINE.equals(type)
            || COMMENT.equals(type)
            || IGNORE.equals(type)) {
          continue;
        } else {
          return forms;
        }
      } else {
        return null;
      }
    }
  }

  private PersistentVector readNumber() {
    for (; ; ) {
      int ch = read1();
      if (ch == -1 || isWhitespace(ch) || isMacro(ch)) {
        unread1();
        break;
      }
    }

    String s = readMarked();
    if (!isNumber(s)) {
      throw new NumberFormatException("Invalid number: " + s);
    }
    return astNode(List.of(NUMBER, s));
  }

  private String readToken() {
    for (; ; ) {
      int ch = read1();
      if (ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch)) {
        unread1();
        return readMarked();
      }
    }
  }

  private PersistentVector tokenToAstNode(String token) {
    switch (token) {
      case "nil":
        return NIL_NODE;
      case "true":
        return astNode(List.of(BOOLEAN, "true"));
      case "false":
        return astNode(List.of(BOOLEAN, "false"));
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
            return astNode(List.of(KEYWORD, token));
          } else {
            return astNode(List.of(SYMBOL, token));
          }
        }
        throw new ReaderException("Invalid token: " + token);
      }
    }
  }

  private PersistentVector readString() {
    for (int ch = read1(); ch != '"'; ch = read1()) {
      if (ch == -1) {
        throw new ReaderException("EOF while reading string");
      }

      if (ch == '\\') {
        //escaped character
        ch = read1();
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
    return astNode(List.of(STRING, s));
  }

  private PersistentVector readComment() {
    while (true) {
      int ch = read1();
      if (ch == -1 || isNewline(ch)) {
        unread1();
        return astNode(List.of(COMMENT, readMarked().trim()));
      }
    }
  }

  private PersistentVector readIgnore() {
    LinkedList forms = readNextIgnoreWhitespace();
    if (forms == null) {
      throw new ReaderException("Unexpected EOF after discard");
    }
    forms.addFirst(IGNORE);
    return astNode(forms);
  }

  private PersistentVector readReaderConditional() {
    int ch = read1();
    boolean splice = false;
    if (ch == '@') {
      splice = true;
    } else {
      unread1();
    }
    LinkedList<PersistentVector> forms = readNextIgnoreWhitespace();
    if (forms == null) {
      throw new ReaderException("Unexpected EOF while reading reader conditional");
    }
    if (!LIST.equals(forms.getLast().seq().first())) {
      throw new ReaderException("Expecting list after reader conditional");
    }
    if (splice) {
      LinkedList inner = forms;
      inner.addFirst(DEREF);
      return astNode(List.of(READER_COND, astNode(inner)));
    } else {
      LinkedList node = forms;
      node.addFirst(READER_COND);
      return astNode(node);
    }
  }


  private PersistentVector readQuote() {
    LinkedList forms = readNextIgnoreWhitespace();
    if (forms == null) {
      throw new ReaderException("Unexpected EOF after quote");
    }
    forms.addFirst(QUOTE);
    return astNode(forms);
  }

  private PersistentVector readSyntaxQuote() {
    LinkedList forms = readNextIgnoreWhitespace();
    if (forms == null) {
      throw new ReaderException("Unexpected EOF after syntax quote");
    }
    forms.addFirst(SYNTAX_QUOTE);
    return astNode(forms);
  }

  private PersistentVector readDeref() {
    LinkedList forms = readNextIgnoreWhitespace();
    if (forms == null) {
      throw new ReaderException("Unexpected EOF after deref");
    }
    forms.addFirst(DEREF);
    return astNode(forms);
  }

  private PersistentVector readMeta() {
    LinkedList<PersistentVector> forms = readNextIgnoreWhitespace();
    if (forms == null) {
      throw new ReaderException("Unexpected EOF after meta character");
    }
    PersistentVector next = forms.getLast();
    Object type = next.seq().first();
    if (!(SYMBOL.equals(type) || MAP.equals(type) || KEYWORD.equals(type) || STRING.equals(type))) {
      throw new ReaderException("Metadata must be Symbol, Keyword, String or Map");
    }
    ((LinkedList) forms).addFirst(META);
    return astNode(forms);
  }

  private PersistentVector readVar() {
    LinkedList forms = readNextIgnoreWhitespace();
    if (forms == null) {
      throw new ReaderException("Unexpected EOF while reading var quote");
    }
    forms.addFirst(VAR_QUOTE);
    return astNode(forms);
  }

  private PersistentVector readRegexp() {
    for (int ch = read1(); ch != '"'; ch = read1()) {
      if (ch == -1)
        throw new ReaderException("EOF while reading regex");
      if (ch == '\\')  //escape
      {
        ch = read1();
        if (ch == -1)
          throw new ReaderException("EOF while reading regex");
      }
    }
    String regexp = readMarked();
    try {
      Pattern.compile(regexp);
    } catch (Exception e) {
      throw new ReaderException("Invalid regex");
    }
    return astNode(List.of(REGEXP, regexp));
  }

  private PersistentVector readUnquote() {
    int ch = read1();
    Keyword type = UNQUOTE;
    if (ch == '@') {
      type = UNQUOTE_SPLICE;
    } else {
      unread1();
    }
    LinkedList forms = readNextIgnoreWhitespace();
    if (forms == null) {
      throw new ReaderException("Unexpected EOF after unquote");
    }
    forms.addFirst(type);
    return astNode(forms);
  }

  private PersistentVector readFn() {
    return readCollection(ANON_FN, '(', ')');
  }

  private PersistentVector readSet() {
    return readCollection(SET, '{', '}');
  }

  private PersistentVector readList() {
    return readCollection(LIST, '(', ')');
  }

  private PersistentVector readVector() {
    return readCollection(VECTOR, '[', ']');
  }

  private PersistentVector readMap() {
    PersistentVector astNode = readCollection(MAP, '{', '}');
    int forms = 0;
    for (Object form : astNode) {
      if (form instanceof PersistentVector) {
        Object type = ((PersistentVector) form).seq().first();
        boolean canSkip = WHITESPACE.equals(type)
            || NEWLINE.equals(type)
            || COMMENT.equals(type)
            || IGNORE.equals(type);
        if (!canSkip) {
          forms++;
        }
      }
    }
    if (forms % 2 != 0) {
      throw new ReaderException("Map literal must contain an even number of forms");
    }
    return astNode;
  }

  private PersistentVector readNsMap() {
    boolean auto = false;
    int autoChar = read1();
    if (autoChar == ':') {
      auto = true;
    } else {
      unread1();
    }

    LinkedList forms = new LinkedList<>();
    PersistentVector next;
    while (true) {
      next = readNext();
      if (next != null) {
        forms.add(next);
        Object type = next.seq().first();
        if (!(WHITESPACE.equals(type) || NEWLINE.equals(type))) {
          break;
        }
      } else {
        throw new ReaderException("EOF while reading namespace map literal");
      }
    }

    if (SYMBOL.equals(next.seq().first())) {
      if (forms.size() > 1) {
        throw new ReaderException("Namespaced map must specify a namespace");
      }
      while (true) {
        next = readNext();
        if (next != null) {
          forms.add(next);
          Object type = next.seq().first();
          if (!(WHITESPACE.equals(type) || NEWLINE.equals(type))) {
            break;
          }
        } else {
          throw new ReaderException("EOF while reading namespace map literal");
        }
      }
      if (!MAP.equals(next.seq().first())) {
        throw new ReaderException("Namespaced map must specify a map");
      }
      forms.addFirst(auto ? "::" : ":");
      forms.addFirst(NAMESPACE_MAP);
      return astNode(forms);
    } else if (MAP.equals(next.seq().first())) {
      if (!auto) {
        throw new ReaderException("Namespaced map must specify a namespace");
      }
      // case #::{}
      forms.removeLast();
      forms.addFirst("::");
      forms.addFirst(NAMESPACE_MAP);
      forms.add(null);
      forms.add(next);
      return astNode(forms);
    } else {
      throw new ReaderException("Namespaced map must specify a map");
    }
  }

  private PersistentVector readCollection(Keyword type, char startChar, char endChar) {
    List forms = new LinkedList();
    forms.add(type);
    pendingEndChars.push((int) endChar);
    try {
      while (true) {
        PersistentVector form = readNext();
        if (form == null) {
          throw new ReaderException("Unmatching paren '" + startChar + "'");
        } else if (form == END_NODE) {
          return astNode(forms);
        } else {
          forms.add(form);
        }
      }
    } finally {
      pendingEndChars.pop();
    }
  }

  private PersistentVector readEval() {
    throw new ReaderException("Eval macro not supported");
  }

  private PersistentVector readChar() {
    mark();
    int ch = read1();
    if (ch == -1) {
      throw new ReaderException("EOF while reading character");
    } else {
      String token = readToken();
      if (token.length() == 1) {
        return astNode(List.of(CHAR, token));
      } else if (token.startsWith("u")) {
        char c = (char) readUnicodeChar(token, 1, 4, 16);
        if (c >= '\ud800' && c <= '\udfff') {
          throw new ReaderException("Invalid character constant: \\u" + Integer.toString(c, 16));
        } else {
          return astNode(List.of(CHAR, token));
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
            return astNode(List.of(CHAR, token));
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
            return astNode(List.of(CHAR, token.charAt(0)));
          default:
            throw new ReaderException("Unsupported character: \\" + token);
        }
      }
    }
  }

  private PersistentVector readArg() {
    int ch = read1();
    if (ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch)) {
      unread1();
      return astNode(List.of(SYMBOL, readMarked()));
    } else if (ch == '&') {
      return astNode(List.of(SYMBOL, readMarked()));
    } else if (Character.isDigit(ch)) {
      while (Character.isDigit(read1())) ;
      unread1();
      return astNode(List.of(SYMBOL, readMarked()));
    } else {
      throw new ReaderException("arg literal must be %, %& or %integer");
    }
  }

  private PersistentVector readDispatch() {
    int ch = read1();
    if (ch == -1) {
      throw new ReaderException("EOF while reading character");
    }
    FormReader reader = ch < dispatchMacros.length ? dispatchMacros[ch] : null;
    if (reader == null) {
      // Try interpret as tagged literal
      unread1();
      LinkedList<PersistentVector> tag = readNextIgnoreWhitespace();
      if (tag == null) {
        throw new ReaderException("EOF while reading dispatch macro");
      }
      if (!SYMBOL.equals(tag.getLast().seq().first())) {
        throw new ReaderException("Reader tag must be a symbol");
      }
      LinkedList<PersistentVector> literal = readNextIgnoreWhitespace();
      if (literal == null) {
        throw new ReaderException("Missing tagged literal value");
      }

      LinkedList forms = tag;
      forms.addAll(literal);
      forms.addFirst(TAGGED_LITERAL);
      return astNode(forms);
    }
    return reader.readNext();
  }

  private PersistentVector readSymbolicValue() {
    LinkedList<PersistentVector> forms = readNextIgnoreWhitespace();
    if (forms == null) {
      throw new ReaderException("Unexpected EOF while reading var quote");
    }
    Object type = forms.getLast().seq().first();
    if (!SYMBOL.equals(type)) {
      throw new ReaderException("Unknown symbolic value");
    }
    String value = (String) forms.getLast().seq().next().first();
    switch (value) {
      case "Inf":
      case "-Inf":
      case "NaN": {
        ((LinkedList) forms).addFirst(SYMBOLIC_VALUE);
        return astNode(forms);
      }
      default:
        throw new ReaderException("Invalid token: ##" + value);
    }
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

  private PersistentVector throwUnmatchedDelimiter() {
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
    return isMacro(ch) ? this.macros[ch] : null;
  }

  private void mark() {
    this._mark = this.index;
  }

  private String readMarked() {
    return this.source.substring(this._mark, this.index);
  }

  private boolean isEOF() {
    return this.index >= this.len;
  }

  private int read1() {
    if (isEOF()) {
      this.index++;
      return -1;
    }
    this.col++;
    return this.source.charAt(this.index++);
  }

  private void unread1() {
    this.col--;
    this.index--;
  }

  private static boolean isNewline(int ch) {
    return ch == '\n';
  }

  private static boolean isWhitespace(int ch) {
    return (Character.isWhitespace(ch) || ch == ',');
  }

  private static PersistentVector astNode(List<?> items) {
    return PersistentVector.create(items);
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
