package imo;

import clojure.lang.Keyword;

public interface Keywords {
  /** Node types **/
  Keyword SPACE = Keyword.intern("space");
  Keyword NEWLINE = Keyword.intern("newline");
  Keyword COMMENT = Keyword.intern("comment");
  Keyword QUOTE = Keyword.intern("quote");
  Keyword SYNTAX_QUOTE = Keyword.intern("syntax-quote");
  Keyword VAR_QUOTE = Keyword.intern("var-quote");
  Keyword DEREF = Keyword.intern("deref");
  Keyword META = Keyword.intern("meta");
  Keyword UNQUOTE = Keyword.intern("unquote");
  Keyword UNQUOTE_SPLICE = Keyword.intern("unquote-splice");
  Keyword DISCARD = Keyword.intern("discard");
  Keyword READER_COND = Keyword.intern("reader-cond");
  Keyword READER_COND_SPLICE = Keyword.intern("reader-cond-splice");
  Keyword SYMBOLIC_VAL = Keyword.intern("symbolic-val");
  Keyword ROOT = Keyword.intern("$");
  Keyword LIST = Keyword.intern("list");
  Keyword VECTOR = Keyword.intern("vector");
  Keyword SET = Keyword.intern("set");
  Keyword MAP = Keyword.intern("map");
  Keyword NS_MAP = Keyword.intern("ns-map");
  Keyword TAGGED_LITERAL = Keyword.intern("tagged-literal");
  Keyword ANON_FN = Keyword.intern("anon-fn");
  Keyword KEYWORD = Keyword.intern("keyword");
  Keyword SYMBOL = Keyword.intern("symbol");
  Keyword NUMBER = Keyword.intern("number");
  Keyword BOOLEAN = Keyword.intern("boolean");
  Keyword CHAR = Keyword.intern("char");
  Keyword STRING = Keyword.intern("string");
  Keyword REGEX = Keyword.intern("regex");
  Keyword NIL = Keyword.intern("nil");

  /** Metadata keywords **/
  Keyword NODE = Keyword.intern("imo/node");
  Keyword LINE = Keyword.intern("line");
  Keyword COL = Keyword.intern("col");
  Keyword HIDDEN = Keyword.intern("hidden");
  Keyword PRE = Keyword.intern("pre");
  Keyword POST = Keyword.intern("post");
}
