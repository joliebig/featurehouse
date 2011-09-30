

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.logging.Level;

public class ParserBlocksData {

    /**
     * Sets the package name for the generated Java code.
     *
     * @layer<bali2javacc>
     */
    public void setPackage( String packageName ) {
        this.packageName = packageName ;
    }

    /**
     * Sets the starting rule name for the generated Java code.
     *
     * @layer<bali2javacc>
     */
    public void setStartRule( String startRule ) {
        this.startRule = startRule ;
    }

    /**
     * Formats parse code blocks by prefixing them with a standard JavaCC
     * template and surrounding the entire result with "PARSER" tags.
     *
     * @layer<bali2javacc>
     */
    public String toString() {

        CodeBuffer code = new CodeBuffer() ;
        code.append( "PARSER_BEGIN(BaliParser)" ) ;
        code.endLine().endLine() ;

        if ( packageName != null && packageName.length() > 0 ) {
            code.append( "package " + packageName + " ;" ) ;
            code.endLine().endLine() ;
        }

        code.append( "public class BaliParser {" ) ;
        code.endLine().endLine() ;

        code.indent() ;

        if ( startRule != null && startRule.length() > 0 ) {

            code.append( "private static " + startRule ) ;
            code.append( "parseRoot = null ;" ) ;
            code.endLine().endLine() ;

            code.append( "public static " + startRule + " getStartRoot" ) ;
            code.append( "() {" ) ;
            code.endLine() ;
            code.indent() ;
            code.append( "return parseRoot ;" ) ;
            code.outdent() ;
            code.endLine().append( '}' ) ;
            code.endLine().endLine() ;

            code.append( "public static " + startRule + " getStartRoot" ) ;
            code.append( "(BaliParser parser)" ) ;
            code.append( "throws ParseException {" ) ;
            code.endLine() ;
            code.indent() ;
            code.append( "try {" ) ;
            code.endLine() ;
            code.indent() ;
            code.append( "parseRoot = parser." + startRule + " () ;" ) ;
            code.endLine().append( "parser.requireEOF () ;" ) ;
            code.endLine().append( "return parseRoot ;" ) ;
            code.outdent() ;
            code.endLine().append( "} catch (TokenMgrError error) {" ) ;
            code.endLine() ;
            code.indent() ;
            code.append( "ParseException e = new ParseException " ) ;
            code.append( "(\"token error occurred\") ;" ) ;
            code.endLine().append( "e.initCause (error) ;" ) ;
            code.endLine().append( "throw e ;" ) ;
            code.outdent() ;
            code.endLine().append( '}' ) ;
            code.outdent() ;
            code.endLine().append( '}' ) ;
            code.endLine().endLine() ;
        }

        code.appendLines( methodLines ) ;
        code.endLine() ;
        code.outdent() ;

        if ( size() > 0 )
            for ( Iterator p = iterator() ; p.hasNext() ; ) {
                code.append( p.next().toString() ) ;
                code.endLine() ;
            }

        code.append( '}' ) ;
        code.endLine().endLine() ;

        code.append( "PARSER_END(BaliParser)" ) ;
        return code.toString() ;
    }

    private String packageName = null ;
    private String startRule = null ;

    private static String methodLines =
        "// Wraps an optional node around an AstNode:"
        + Main.LINE_SEPARATOR
        + "//"
        + Main.LINE_SEPARATOR
        + "static AstOptNode opt (AstNode node) {"
        + Main.LINE_SEPARATOR
        + "    return new AstOptNode () . setParms (node) ;"
        + Main.LINE_SEPARATOR
        + "}"
        + Main.LINE_SEPARATOR
        + Main.LINE_SEPARATOR
        + "// Wraps an optional node around an AstToken:"
        + Main.LINE_SEPARATOR
        + "//"
        + Main.LINE_SEPARATOR
        + "static AstOptToken opt (AstToken token) {"
        + Main.LINE_SEPARATOR
        + "    return new AstOptToken () . setParms (token) ;"
        + Main.LINE_SEPARATOR
        + "}"
        + Main.LINE_SEPARATOR
        + Main.LINE_SEPARATOR
        + "// Forces an end-of-file check in the tokenStream:"
        + Main.LINE_SEPARATOR
        + "//"
        + Main.LINE_SEPARATOR
        + "public void requireEOF () throws ParseException {"
        + Main.LINE_SEPARATOR
        + "    try {"
        + Main.LINE_SEPARATOR
        + "        jj_consume_token (BaliParserConstants.EOF) ;"
        + Main.LINE_SEPARATOR
        + "    } catch (TokenMgrError error) {"
        + Main.LINE_SEPARATOR
        + "        ParseException e = new ParseException (\"EOF error\") ;"
        + Main.LINE_SEPARATOR
        + "        e.initCause (error) ;"
        + Main.LINE_SEPARATOR
            + "        throw e ;"
        + Main.LINE_SEPARATOR
            + "    }"
        + Main.LINE_SEPARATOR
        + "}"
        + Main.LINE_SEPARATOR
        + Main.LINE_SEPARATOR
        + "// Converts a JavaCC Token to a Bali AstToken:"
        + Main.LINE_SEPARATOR
        + "//"
        + Main.LINE_SEPARATOR
        + "static AstToken t2at (Token tok) {"
        + Main.LINE_SEPARATOR
        + Main.LINE_SEPARATOR
        + "    // Special case -- if token is optional:"
        + Main.LINE_SEPARATOR
        + "    //"
        + Main.LINE_SEPARATOR
        + "    if (tok == null)"
        + Main.LINE_SEPARATOR
        + "        return (null) ;"
        + Main.LINE_SEPARATOR
        + Main.LINE_SEPARATOR
        + "    StringBuffer buffer = new StringBuffer () ;"
        + Main.LINE_SEPARATOR
        + "    Token special = tok.specialToken;"
        + Main.LINE_SEPARATOR
        + "    while (special != null) {"
        + Main.LINE_SEPARATOR
        + "        buffer.insert (0, special.toString()) ;"
        + Main.LINE_SEPARATOR
        + "        special = special.specialToken ;"
        + Main.LINE_SEPARATOR
        + "    }"
        + Main.LINE_SEPARATOR
        + "    String white = buffer.toString () ;"
        + Main.LINE_SEPARATOR
        + Main.LINE_SEPARATOR
        + "    return new AstToken().setParms"
        + " (white, tok.image, tok.endLine) ;"
        + Main.LINE_SEPARATOR
        + "}" ;

}
