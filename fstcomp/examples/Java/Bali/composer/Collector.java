layer composer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Level;

//-----------------------------------------------------------------------//
// Composer-specific Collector class:
//-----------------------------------------------------------------------//

public refines class Collector {

    public String toString() {
        return
                header( "Option block" )
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
                + header( "Bali tokens" )
                + baliTokens
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Regular expression tokens" )
                + regexTokens
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Java code blocks" )
                + javaBlocks
                + Main.LINE_SEPARATOR
                + Main.LINE_SEPARATOR
                + header( "Bali productions" )
                + baliRules ;
    }

    static private String header( String message ) {
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

}
