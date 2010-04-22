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

public refines class ParserBlocksData {

    /**
     * Formats parse code blocks by concatenating them with blank line
     * separators and with "code {" prefix and "} code" suffix.
     *
     * @layer<composer>
     */
    public String toString() {

        StringBuffer buffer = new StringBuffer( "code {" ) ;

        for ( Iterator p = iterator() ; p.hasNext() ; )
            buffer.append( p.next() ).append( Main.LINE_SEPARATOR ) ;

        buffer.append( "} code" ) ;
        return buffer.toString() ;
    }

}
