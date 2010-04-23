

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

//-----------------------------------------------------------------------//
// Main class:
//-----------------------------------------------------------------------//

/**
 * Provides a <code>driver</code> method to test the data collection of
 * this layer.  The <code>driver</code> chains to the super class'
 * <code>driver</code> method to obtain a parse tree which is then visited
 * by a {@link Collector}.  The data collected is then examined for
 * correctness.
 *
 * @layer<collect>
 */
    
public class Main {

    public Object driver( String[] args ) throws Throwable {
        Main.DEBUG.entering( "collect.Main", "driver", args ) ;
        setVersion( "v2002.09.03" ) ;

        BaliParse parseTree =
                ( BaliParse ) original( args ) ;

        Collector collector = new Collector() ;
        collector.dispatch( parseTree ) ;

        Main.DEBUG.info( "Bali Rules: " + collector.baliRules ) ;
        Main.DEBUG.info( "Bali Tokens: " + collector.baliTokens ) ;
        Main.DEBUG.info( "Javacode Blocks: " + collector.javaBlocks ) ;
        Main.DEBUG.info( "Options: " + collector.optionBlocks ) ;
        Main.DEBUG.info( "Parser Code: " + collector.parserBlocks ) ;
        Main.DEBUG.info( "Regex Tokens: " + collector.regexTokens ) ;
        Main.DEBUG.info( "Token Manager Code: " + collector.tknMgrBlocks ) ;

        Main.DEBUG.exiting( "collect.Main", "driver", "parseTree" ) ;
        return parseTree ;
    }

}
