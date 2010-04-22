layer bali2javacc;

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

//-----------------------------------------------------------------------//
// Translator-specific Collector class:
//-----------------------------------------------------------------------//

public refines class Collector {

    public void setPackage( String packageName ) {
        parserBlocks.setPackage( packageName ) ;
    }

    public String toString() {
        parserBlocks.setStartRule( baliRules.getStartName() ) ;
        return
                header( "Options block" )
                + optionBlocks
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Parser code block" )
                + parserBlocks
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Token manager declarations" )
                + tknMgrBlocks
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Standard token definitions" )
                + standardTokens()
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Bali tokens from grammar" )
                + baliTokens
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Regular-expression tokens from grammar" )
                + regexTokens
                + Main.LINE_SEPARATOR
                + header( "JAVACODE blocks from grammar" )
                + javaBlocks
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Productions from Bali grammar" )
                + baliRules
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Other standard tokens:" )
                + otherTokens() ;
    }

    private static String header( String message ) {
        return
                "//-----------------------------------//"
                + Main.LINE_SEPARATOR
                + "// "
                + message + ':'
                + Main.LINE_SEPARATOR
                + "//-----------------------------------//"
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR ;
    }

    private static String otherTokens() {
        return
                "TOKEN : {"
                + Main.LINE_SEPARATOR
                + "    <IDENTIFIER: <LETTER> (<LETTER> | <DIGIT>)*>"
                + Main.LINE_SEPARATOR
                + "    | <OTHER: ~[]>"
                + Main.LINE_SEPARATOR
                + "}" ;
    }

    private static String standardTokens() {
        return
                "SPECIAL_TOKEN : {\" \"|\"\\f\"|\"\\n\"|\"\\r\"|\"\\t\"}"
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + "// COMMENTS:"
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + "MORE : {"
                + Main.LINE_SEPARATOR
                + "    \"//\" : IN_SINGLE_LINE_COMMENT"
                + Main.LINE_SEPARATOR
                + "    | \"@\"  : IN_SINGLE_LINE_COMMENT"
                + Main.LINE_SEPARATOR
                + "    | <\"/**\" ~[\"/\"]> { input_stream.backup(1); } :"
                + " IN_FORMAL_COMMENT"
                + Main.LINE_SEPARATOR
                + "    | \"/*\" : IN_MULTI_LINE_COMMENT"
                + Main.LINE_SEPARATOR
                + "}"
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + "<IN_SINGLE_LINE_COMMENT>"
                + Main.LINE_SEPARATOR
                + "SPECIAL_TOKEN : {"
                + Main.LINE_SEPARATOR
                + "    <SINGLE_LINE_COMMENT: "
                + "\"\\n\""
                + " | \"\\n\\r\""
                + " | \"\\r\""
                + " | \"\\r\\n\""
                + "> : DEFAULT"
                + Main.LINE_SEPARATOR
                + "}"
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + "<IN_FORMAL_COMMENT>"
                + Main.LINE_SEPARATOR
                + "SPECIAL_TOKEN : {"
                + Main.LINE_SEPARATOR
                + "    <FORMAL_COMMENT: \"*/\" > : DEFAULT"
                + Main.LINE_SEPARATOR
                + "}"
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + "<IN_MULTI_LINE_COMMENT>"
                + Main.LINE_SEPARATOR
                + "SPECIAL_TOKEN : {"
                + Main.LINE_SEPARATOR
                + "    <MULTI_LINE_COMMENT: \"*/\" > : DEFAULT"
                + Main.LINE_SEPARATOR
                + "}"
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + "<IN_SINGLE_LINE_COMMENT,IN_FORMAL_COMMENT,"
                + "IN_MULTI_LINE_COMMENT>"
                + Main.LINE_SEPARATOR
                + "MORE : { < ~[] > }"
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + "TOKEN : {"
                + Main.LINE_SEPARATOR
                + "    <#LETTER: [\"a\"-\"z\", \"A\"-\"Z\", \"_\", \"$\"]>"
                + Main.LINE_SEPARATOR
                + "    | <#DIGIT: [\"0\"-\"9\"]>"
                + Main.LINE_SEPARATOR
                + "}" ;
        
    }

}
