package imo;

import clojure.lang.*;

import java.util.*;

import static imo.Keywords.*;

public final class AstNode {

  public static AstNode createRoot(LinkedList<AstNode> children) {
    return new AstNode(1, 1, ROOT, children, 0, 0);
  }

  public static AstNode createList(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, LIST, children, 2, 0);
  }

  public static AstNode createVector(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, VECTOR, children, 2, 0);
  }

  public static AstNode createSet(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, SET, children, 3, 0);
  }

  public static AstNode createMap(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, MAP, children, 2, 0);
  }

  public static AstNode createNsMap(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, NS_MAP, children, 1, 0);
  }

  public static AstNode createTaggedLiteral(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, TAGGED_LITERAL, children, 1, 0);
  }

  public static AstNode createAnonFn(int line, int col, List<AstNode> children) {
    return new AstNode(line, col, ANON_FN, children, 3, 0);
  }

  public static AstNode createQuote(int line, int col, AstNode inner) {
    return new AstNode(line, col, QUOTE, List.of(inner), 1, 0);
  }

  public static AstNode createSyntaxQuote(int line, int col, AstNode inner) {
    return new AstNode(line, col, SYNTAX_QUOTE, List.of(inner), 1, 0);
  }

  public static AstNode createVarQuote(int line, int col, AstNode inner) {
    return new AstNode(line, col, VAR_QUOTE, List.of(inner), 1, 0);
  }

  public static AstNode createDeref(int line, int col, AstNode inner) {
    return new AstNode(line, col, DEREF, List.of(inner), 1, 0);
  }

  public static AstNode createMeta(int line, int col, AstNode inner) {
    return new AstNode(line, col, META, List.of(inner), 1, 0);
  }

  public static AstNode createUnquote(int line, int col, AstNode inner) {
    return new AstNode(line, col, UNQUOTE, List.of(inner), 1, 0);
  }

  public static AstNode createUnquoteSplice(int line, int col, AstNode inner) {
    return new AstNode(line, col, UNQUOTE_SPLICE, List.of(inner), 2, 0);
  }

  public static AstNode createDiscard(int line, int col, AstNode inner) {
    return new AstNode(line, col, DISCARD, List.of(inner), 2, 0);
  }

  public static AstNode createReaderCond(int line, int col, AstNode inner) {
    return new AstNode(line, col, READER_COND, List.of(inner), 2, 0);
  }

  public static AstNode createReaderCondSplice(int line, int col, AstNode inner) {
    return new AstNode(line, col, READER_COND_SPLICE, List.of(inner), 3, 0);
  }

  public static AstNode createSymbolicVal(int line, int col, AstNode inner) {
    return new AstNode(line, col, SYMBOLIC_VAL, List.of(inner), 2, 0);
  }

  public static AstNode createKeyword(int line, int col, String content) {
    return new AstNode(line, col, KEYWORD, List.of(content), content.length(), 0);
  }

  public static AstNode createSymbol(int line, int col, String content) {
    return new AstNode(line, col, SYMBOL, List.of(content), content.length(), 0);
  }

  public static AstNode createNumber(int line, int col, String content) {
    return new AstNode(line, col, NUMBER, List.of(content), content.length(), 0);
  }

  public static AstNode createBoolean(int line, int col, boolean value) {
    String content = value ? "true" : "false";
    return new AstNode(line, col, BOOLEAN, List.of(content), content.length(), 0);
  }

  public static AstNode createChar(int line, int col, String content) {
    return new AstNode(line, col, CHAR, List.of(content), content.length(), 0);
  }

  public static AstNode createString(int line, int col, String content) {
    // todo: string stats here, line break as well!
    return new AstNode(line, col, STRING, List.of(content), content.length(), 0);
  }

  public static AstNode createRegex(int line, int col, String content) {
    return new AstNode(line, col, REGEX, List.of(content), content.length(), 0);
  }

  public static AstNode createNil(int line, int col) {
    return new AstNode(line, col, NIL, List.of("nil"), 3, 0);
  }

  public static AstNode createSpace(int line, int col, String content) {
    return new AstNode(line, col, SPACE, List.of(content), 0, 0);
  }

  public static AstNode createNewline(int line, int col) {
    return new AstNode(line, col, NEWLINE, List.of("\n"), 0, 0);
  }

  public static AstNode createComment(int line, int col, String content) {
    return new AstNode(line, col, COMMENT, List.of(content), 0, 1);
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

  public static Collection<String> getAllBeginChars() {
    return _beginChars.values();
  }

  public static Collection<String> getAllEndChars() {
    return _endChars.values();
  }

  //
  //
  //

  public final int line;
  public final int col;
  public final Keyword type;
  public final List<?> children;
  public final int selfLength;
  public final int selfLines;

  public List<AstNode> pre;
  public List<AstNode> post;
  public List<AstNode> metaChildren;

  public AstNode(int line, int col, Keyword type, List<?> children, int selfLength, int selfLines) {
    this.line = line;
    this.col = col;
    this.type = type;
    this.children = children;
    this.selfLength = selfLength;
    this.selfLines = selfLines;
  }

  public AstNode(int line, int col, Keyword type, List<?> children, int selfLength, int selfLines, ITransientMap meta) {
    this.line = line;
    this.col = col;
    this.type = type;
    this.children = children;
    this.selfLength = selfLength;
    this.selfLines = selfLines;
  }

  public PersistentVector toVec() {
    ITransientCollection vec = PersistentVector.EMPTY.asTransient();
    int innerLength = selfLength + Math.max(0, children.size() - 1);
    int innerLines = selfLines;
    vec = vec.conj(type);
    for (Object child : children) {
      if (child instanceof AstNode) {
        PersistentVector childVec = ((AstNode) child).toVec();
        IPersistentMap childMeta = childVec.meta();
        innerLength += (Integer) childMeta.valAt(OUTER_LENGTH);
        innerLines += (Integer) childMeta.valAt(OUTER_LINES);
        vec = vec.conj(childVec);
      } else {
        vec = vec.conj(child);
      }
    }

    ITransientMap meta = PersistentArrayMap.EMPTY
        .asTransient()
        .assoc(NODE, true)
        .assoc(LINE, line)
        .assoc(COL, col)
        .assoc(INNER_LENGTH, innerLength)
        .assoc(OUTER_LENGTH, innerLength)
        .assoc(INNER_LINES, innerLines)
        .assoc(OUTER_LINES, innerLines);

    meta = assocNodes(meta, PRE, pre, true, false);
    meta = assocNodes(meta, CHILDREN, metaChildren, children.size() > 0, true);
    meta = assocNodes(meta, POST, post, true, false);
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
    appendNodeStrings(sb, metaChildren);
    if (endChars != null) {
      sb.append(endChars);
    }
    appendNodeStrings(sb, post);
    return sb.toString();
  }

  private static ITransientMap assocNodes(ITransientMap meta, Keyword key, List<AstNode> nodes, boolean needsSpace, boolean isInner) {
    if (nodes != null && !nodes.isEmpty()) {
      ITransientCollection res = PersistentVector.EMPTY.asTransient();
      int lines = 0;
      int length = 0;
      for (AstNode node : nodes) {
        PersistentVector vec = node.toVec();
        IPersistentMap m = vec.meta();
        lines += (Integer) m.valAt(OUTER_LINES);
        length += (Integer) m.valAt(OUTER_LENGTH);
        res = res.conj(vec);
      }
      if (lines > 0) {
        if (isInner) {
          meta = meta.assoc(INNER_LINES, lines + (Integer) meta.valAt(INNER_LINES));
        }
        meta = meta.assoc(OUTER_LINES, lines + (Integer) meta.valAt(OUTER_LINES));
      }
      if (length > 0) {
        if (isInner) {
          meta = meta.assoc(INNER_LENGTH, (needsSpace ? 1 : 0) + length + (Integer) meta.valAt(INNER_LENGTH));
        }
        meta = meta.assoc(OUTER_LENGTH, (needsSpace ? 1 : 0) + length + (Integer) meta.valAt(OUTER_LENGTH));
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
