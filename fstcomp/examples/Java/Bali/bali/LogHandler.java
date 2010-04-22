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

//-----------------------------------------------------------------------//
// Logging classes and instances:
//-----------------------------------------------------------------------//

/**
 * Provides an implementation of {@link LogHandler} that prints onto
 * standard output (a "console").  Also provides an instance for
 * centralized logging output.
 *
 * @layer<bali>
 */
    
class LogHandler extends ConsoleHandler {

    final public static LogHandler CONSOLE = new LogHandler( "console" ) ;

    /**
     * Overrides {@link ConsoleHandler#publish(LogRecord)} to print a
     * labelled, single-line log message on {@link System#err}.
     *
     * @layer<bali>
     */
    final public void publish( LogRecord record ) {
        record.setMessage( prefix + record.getMessage() ) ;
        super.publish( record ) ;
    }

    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    /**
     * Private constructor to prevent external instantiation.
     *
     * @layer<bali>
     */
    private LogHandler( String label ) {
            Super( String )() ;
        prefix = label + ": " ;
        setLevel( Level.ALL ) ;
    }

    final private String prefix ;
}
