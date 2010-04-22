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

public refines class BaliTokensData {

    /**
     * Formats the Bali tokens in alphabetical order by name (key) with
     * each definition on a single line.  The line has the quoted string
     * value first and the name second.  Further, an attempt is made to
     * place the values and names in left-justified columns.
     *
     * @layer<composer>
     */
    public String toString() {

        List keys = new ArrayList( keySet() ) ;
        Collections.sort( keys ) ;

        StringBuffer buffer = new StringBuffer() ;

        for ( Iterator p = keys.iterator() ; p.hasNext() ; ) {
            String key = ( String ) p.next() ;
            String value = ( String ) get( key ) ;
            buffer.append( value ) ;

            for ( int k = value.length() ; k < 16 ; ++k )
                buffer.append( ' ' ) ;
            buffer.append( "\t" ).append( key ) ;

            buffer.append( Main.LINE_SEPARATOR ) ;
        }

        return buffer.toString() ;
    }

}
