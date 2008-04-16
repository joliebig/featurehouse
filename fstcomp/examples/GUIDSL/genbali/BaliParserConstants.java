
public interface BaliParserConstants {

  int EOF = 0;
  int SINGLE_LINE_COMMENT = 9;
  int FORMAL_COMMENT = 10;
  int MULTI_LINE_COMMENT = 11;
  int LETTER = 13;
  int DIGIT = 14;
  int STRING_LITERAL = 15;
  int IDENTIFIER = 39;
  int OTHER = 40;

  int DEFAULT = 0;
  int IN_SINGLE_LINE_COMMENT = 1;
  int IN_FORMAL_COMMENT = 2;
  int IN_MULTI_LINE_COMMENT = 3;

  String[] tokenImage = {
    "<EOF>",
    "\" \"",
    "\"\\f\"",
    "\"\\n\"",
    "\"\\r\"",
    "\"\\t\"",
    "\"//\"",
    "<token of kind 7>",
    "\"/*\"",
    "<SINGLE_LINE_COMMENT>",
    "\"*/\"",
    "\"*/\"",
    "<token of kind 12>",
    "<LETTER>",
    "<DIGIT>",
    "<STRING_LITERAL>",
    "\"and\"",
    "\"{\"",
    "\"}\"",
    "\"(\"",
    "\")\"",
    "\"%%\"",
    "\"iff\"",
    "\"choose1\"",
    "\",\"",
    "\";\"",
    "\"let\"",
    "\":\"",
    "\"+\"",
    "\"*\"",
    "\"[\"",
    "\"]\"",
    "\"implies\"",
    "\"not\"",
    "\"or\"",
    "\"=\"",
    "\"::\"",
    "\"|\"",
    "\"##\"",
    "<IDENTIFIER>",
    "<OTHER>",
  };

}
