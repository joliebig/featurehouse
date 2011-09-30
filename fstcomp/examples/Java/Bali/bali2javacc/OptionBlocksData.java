

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

public class OptionBlocksData {

    /**
     * Formats options blocks by concatenating them with blank line
     * separators and with "options {" prefix and "}" suffix.
     *
     * @layer<bali2javacc>
     */
    public String toString() {

        if ( size() < 1 )
            return "// No options blocks defined in Bali grammar." ;

        StringBuffer buffer = new StringBuffer( "options {" ) ;

        for ( Iterator p = iterator() ; p.hasNext() ; )
            buffer.append( p.next() ).append( Main.LINE_SEPARATOR ) ;

        buffer.append( "}" ) ;
        return buffer.toString() ;
    }

}
