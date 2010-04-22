layer visitor;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

//-------------------------------------------------------------------------
// Main class:
//-------------------------------------------------------------------------

/**
 * Provides a <code>driver</code> method that chains to the super class'
 * <code>driver</code> method to obtain a parse tree which is visited by a
 * test version of an {@link AstVisitor}.  This version implements a test
 * that walks the parse tree, printing labels of {@link BaliGrammarNode}
 * nodes -- these are the names of the grammar rules.
 *
 * @layer<visitor>
 */
    
public refines class Main {

    public Object driver( String[] args ) throws Throwable {

        DEBUG.entering( "visitor.Main", "driver", args ) ;
        setVersion( "v2002.09.04" ) ;

        BaliParse parseTree =
                ( BaliParse ) Super( String[] ).driver( args ) ;

        MainVisitor visitor = new MainVisitor() ;
        visitor.dispatch( parseTree ) ;

        if ( ! targetRules.equals( visitor.rules ) ) {
            System.err.println( targetRules ) ;
            System.err.println( visitor.rules ) ;
            throw new IllegalStateException( "rule names differ" ) ;
        }
            
        DEBUG.exiting( "visitor.Main", "driver", "parseTree" ) ;
        return parseTree ;
    }

    final public List targetRules = Arrays.asList( new String []
        {"BaliParse", "Options", "ParserCode", "Block", "Statements",
         "Statement", "BaliTokenDefinition", "JavacodeProduction",
         "TokenManagerDeclarations", "ScanBlock", "BaliGrammarRule",
         "Productions", "Production", "Lookahead", "Rewrite",
         "PrimitiveRewrite", "Pattern", "ClassName", "Primitive",
         "Terminal", "RegexTokenDefinition", "StateSet", "StatesSpecifier",
         "StatesList", "StateName", "REKind", "CaseFlag", "REList",
         "RegexBlock", "NextState", "Regex", "AngleRegex", "ComplexRegex",
         "Label"} ) ;
}
