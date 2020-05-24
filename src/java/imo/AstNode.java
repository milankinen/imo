package imo;

import clojure.lang.*;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static imo.Keywords.*;

public final class AstNode {

  public static AstNode createRoot(LinkedList<AstNode> children) {
    return new AstNode(1, 1, ROOT, children);
  }

  public static AstNode createList(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, LIST, children);
  }

  public static AstNode createVector(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, VECTOR, children);
  }

  public static AstNode createSet(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, SET, children);
  }

  public static AstNode createMap(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, MAP, children);
  }

  public static AstNode createNsMap(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, NS_MAP, children);
  }

  public static AstNode createTaggedLiteral(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, TAGGED_LITERAL, children);
  }

  public static AstNode createAnonFn(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, ANON_FN, children);
  }

  public static AstNode createQuote(int line, int col, AstNode inner) {
    return new AstNode(line, col, QUOTE, List.of(inner));
  }

  public static AstNode createSyntaxQuote(int line, int col, AstNode inner) {
    return new AstNode(line, col, SYNTAX_QUOTE, List.of(inner));
  }

  public static AstNode createVarQuote(int line, int col, AstNode inner) {
    return new AstNode(line, col, VAR_QUOTE, List.of(inner));
  }

  public static AstNode createDeref(int line, int col, AstNode inner) {
    return new AstNode(line, col, DEREF, List.of(inner));
  }

  public static AstNode createMeta(int line, int col, AstNode inner) {
    return new AstNode(line, col, META, List.of(inner));
  }

  public static AstNode createUnquote(int line, int col, AstNode inner) {
    return new AstNode(line, col, UNQUOTE, List.of(inner));
  }

  public static AstNode createUnquoteSplice(int line, int col, AstNode inner) {
    return new AstNode(line, col, UNQUOTE_SPLICE, List.of(inner));
  }

  public static AstNode createDiscard(int line, int col, AstNode inner) {
    return new AstNode(line, col, DISCARD, List.of(inner));
  }

  public static AstNode createReaderCond(int line, int col, AstNode inner) {
    return new AstNode(line, col, READER_COND, List.of(inner));
  }

  public static AstNode createReaderCondSplice(int line, int col, AstNode inner) {
    return new AstNode(line, col, READER_COND_SPLICE, List.of(inner));
  }

  public static AstNode createSymbolicVal(int line, int col, AstNode inner) {
    return new AstNode(line, col, SYMBOLIC_VAL, List.of(inner));
  }

  public static AstNode createKeyword(int line, int col, String content) {
    return new AstNode(line, col, KEYWORD, List.of(content));
  }

  public static AstNode createSymbol(int line, int col, String content) {
    return new AstNode(line, col, SYMBOL, List.of(content));
  }

  public static AstNode createNumber(int line, int col, String content) {
    return new AstNode(line, col, NUMBER, List.of(content));
  }

  public static AstNode createBoolean(int line, int col, boolean value) {
    return new AstNode(line, col, BOOLEAN, List.of(value ? "true" : "false"));
  }

  public static AstNode createChar(int line, int col, String content) {
    return new AstNode(line, col, CHAR, List.of(content));
  }

  public static AstNode createString(int line, int col, String content) {
    return new AstNode(line, col, STRING, List.of(content));
  }

  public static AstNode createRegex(int line, int col, String content) {
    return new AstNode(line, col, REGEX, List.of(content));
  }

  public static AstNode createNil(int line, int col) {
    return new AstNode(line, col, NIL, List.of("nil"));
  }

  public static AstNode createSpace(int line, int col, String content) {
    return new AstNode(line, col, SPACE, List.of(content));
  }

  public static AstNode createNewline(int line, int col) {
    return new AstNode(line, col, NEWLINE, List.of("\n"));
  }

  public static AstNode createComment(int line, int col, String content) {
    return new AstNode(line, col, COMMENT, List.of(content));
  }

  private static final Map<Keyword, String> _beginChars = new HashMap<>();
  private static final Map<Keyword, String> _endChars = new HashMap<>();

  static {
    _beginChars.put(LIST, "(");
    _beginChars.put(VECTOR, "[");
    _beginChars.put(MAP, "{");
    _beginChars.put(SET, "#{");
    _beginChars.put(NS_MAP, "#");
    _beginChars.put(TAGGED_LITERAL, "#");
    _beginChars.put(QUOTE, "'");
    _beginChars.put(SYNTAX_QUOTE, "`");
    _beginChars.put(VAR_QUOTE, "#'");
    _beginChars.put(UNQUOTE, "~");
    _beginChars.put(UNQUOTE_SPLICE, "~@");
    _beginChars.put(ANON_FN, "#(");
    _beginChars.put(DEREF, "@");
    _beginChars.put(META, "^");
    _beginChars.put(DISCARD, "#_");
    _beginChars.put(READER_COND, "#?");
    _beginChars.put(READER_COND_SPLICE, "#?@");
    _beginChars.put(SYMBOLIC_VAL, "##");

    _endChars.put(LIST, ")");
    _endChars.put(VECTOR, "]");
    _endChars.put(MAP, "}");
    _endChars.put(SET, "}");
    _endChars.put(ANON_FN, ")");
  }

  public static String getBeginChars(Keyword nodeType) {
    return _beginChars.get(nodeType);
  }

  public static String getEndChars(Keyword nodeType) {
    return _endChars.get(nodeType);
  }

  //
  //
  //

  public final int line;
  public final int col;
  public final Keyword type;
  public final List<?> children;

  public List<AstNode> pre;
  public List<AstNode> post;
  public List<AstNode> hidden;

  public AstNode(int line, int col, Keyword type, List<?> children) {
    this.line = line;
    this.col = col;
    this.type = type;
    this.children = children;
  }

  public PersistentVector toVec() {
    ITransientCollection vec = PersistentVector.EMPTY.asTransient();
    vec = vec.conj(type);
    for (Object child : children) {
      if (child instanceof AstNode) {
        vec = vec.conj(((AstNode) child).toVec());
      } else {
        vec = vec.conj(child);
      }
    }
    ITransientMap meta = PersistentArrayMap.EMPTY
        .asTransient()
        .assoc(NODE, true)
        .assoc(LINE, line)
        .assoc(COL, col);
    meta = assocNodes(meta, PRE, pre);
    meta = assocNodes(meta, HIDDEN, hidden);
    meta = assocNodes(meta, POST, post);
    return ((PersistentVector) vec.persistent()).withMeta(meta.persistent());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    String beginChars = getBeginChars(type);
    String endChars = getEndChars(type);
    appendNodeStrings(sb, pre);
    if (beginChars != null) {
      sb.append(beginChars);
    }
    for (Object child : children) {
      sb.append(child.toString());
    }
    appendNodeStrings(sb, hidden);
    if (endChars != null) {
      sb.append(endChars);
    }
    appendNodeStrings(sb, post);
    return sb.toString();
  }

  private static ITransientMap assocNodes(ITransientMap meta, Keyword key, List<AstNode> nodes) {
    if (nodes != null && !nodes.isEmpty()) {
      ITransientCollection res = PersistentVector.EMPTY.asTransient();
      for (AstNode node : nodes) {
        res = res.conj(node.toVec());
      }
      return meta.assoc(key, res.persistent().seq());
    } else {
      return meta;
    }
  }

  private static void appendNodeStrings(StringBuilder sb, List<AstNode> nodes) {
    if (nodes != null) {
      for (AstNode node : nodes) {
        sb.append(node.toString());
      }
    }
  }
}
