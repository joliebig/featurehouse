layer bali;

import Jakarta.util.Util;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.LogRecord;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

public abstract refines class AstList {

    public void execute() {
        for ( AstNode node = arg[0]; node != null; node = node.right )
            if ( node.arg[0] != null )
                node.arg[0].execute() ;
    }

    /**
     * Returns an unmodifiable {@link List} of the nodes in this
     * <code>AstList</code>.  <em>Note:</em> the actual nodes are stored,
     * <em>not</em> the intermediate {@link AstListNode} elements (which
     * are present only as data holders).  Further, the value of an actual
     * node may be <code>null</code>, so there may be <code>null</code>
     * elements in the returned {@link List}.
     *
     * <p>
     * After the {@link List} is first computed, it is cached and returned
     * by later calls to this method.  Therefore, if the nodes linked here
     * are modified, the cached copy should be set to <code>null</code>
     * so that the next call will construct a new copy.
     *
     * @layer<bali>
     */
    public List toList() {

        if ( nodesAsList == null ) {

            nodesAsList = new ArrayList() ;
            for ( AstNode nd = arg [0] ; nd != null ; nd = nd.right )
                nodesAsList.add( nd.arg [0] ) ;

            nodesAsList = Collections.unmodifiableList( Arrays.asList( nodesAsList.toArray( new AstNode [nodesAsList.size()] ) ) ) ;
        }

        return nodesAsList ;
    }

    private List nodesAsList = null ;
}
