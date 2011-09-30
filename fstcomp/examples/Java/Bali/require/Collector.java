

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Extend {@link collector} to handle "require" statement references,
 * rule name definitions and ordinary rule references.
 *
 * @layer<require>
 */
    
public class Collector {

    public void visit( BaliGrammarNode node ) {
        original( node ) ;
        baliRules.addDefinition( node.tok[0].getTokenName() ) ;

        // Visit sub-trees to capture rule references:
        //
        visit( ( AstNode ) node ) ;
    }

    public void visit( IdentifierNode node ) {
        baliRules.addReference( node.tok[0].getTokenName() ) ;
    }

    public void visit( RequireRuleNode node ) {

        String ruleName = node.tok[0].getTokenName() ;
        baliRules.addRequire( ruleName ) ;

        RequireTypeNode type = ( RequireTypeNode ) node.arg[0].arg [0] ;
        if ( type == null )
            return ;

        baliRules.setType( ruleName, type.tok[1].getTokenName() ) ;
    }

}
