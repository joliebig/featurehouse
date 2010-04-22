

import java.io.PrintWriter;

//**************************************************
// NameId extension class
//**************************************************
    
public class NameId {
    public  AstNode myAlias;

    // setParms not needed because it would only call setParms of the parent

    public  AstNode setParms( Environment env,  AstTokenInterface _arg0 ) {

        setParms( ( AstToken ) _arg0 );
        checkMangleOrAlias( env );
        return ( NameId ) this ;
    }

    // For use with findNameId().
    private static  AstCursor csr = new  AstCursor();

    //**************************************************
    // Utility to locate a NameId node and return the string corresponding
    // to its IDENTIFIER.
    //**************************************************
    public static String findNameId( AstNode node ) {
        AstTokenInterface t;

        if ( node instanceof  NameId )
            return ( node.tok[0].tokenName() );
        csr.First( node );
        csr.PlusPlus();
        while ( csr.More() ) {
            if ( csr.node instanceof  NameId ) {
                t = csr.node.tok[0];
                return ( t.tokenName() );
            }
            csr.PlusPlus();
        }
        return ( null );
    }

    private void checkMangleOrAlias( Environment env ) {
        EnvElem elem;
        AstToken id_token;

        if ( env == null )
            return; // do nothing

        id_token = ( AstToken ) tok[0];
        elem = ( EnvElem ) env.findId( id_token.name );
        if ( elem == null ) {
            return; // do nothing
        }

        if ( elem._alias != null ) {
            // Save a reference to this object on the alias stack so
            // that patch can replace it with its alias.
            aliasStack.push( this );
            myAlias = elem._alias;
            return;
        }

        // Else, mangle name
        id_token.name += elem.mangleNum();
    }

    public void doPatch() {
        AstNode alias_ast;

        // do the patch

        // Normally we replace the current node with the AST_Exp contained
        // in the AST_Exp unless:
        //        1) the parent node is an instance of AstListNode
        //        2) the AST_Exp is an instance of AstList
        // in which case we'll merge the lists.

        alias_ast = ( AstNode ) myAlias.clone();
        if ( ( up instanceof AstListNode ) &&
                ( alias_ast instanceof AstList ) ) {
            // merge lists
            up.AddAfter( ( AstList ) alias_ast );
            up.Delete();
        }
        else {
            // replacement
            this.Replace( alias_ast );
        }
    }

    public void reduce2ast( AstProperties props ) {
        boolean order[];
        int t, n, i;
        PrintWriter ps;
        Integer level;

        order = printorder();
        t = 0;
        n = 0;
        ps = ( PrintWriter ) props.getProperty( "output" );

        // Get level number
        level = ( Integer ) props.getProperty( "AstLevel" );

        ps.print( " (" + className() + ") new " + className() +
                     "().setParms(" );
        if ( ( props.getProperty( "env" ) != null ) && ( level.intValue() == 1 ) )
            ps.print( "_E, " );
        for ( i=0; i<order.length; i++ ) {
            if ( i>0 )
                ps.print( ", " );
            // if order[i] is true; print token else print nonterminal

            if ( order[i] )
                tok[t++].reduce2ast( props );
            else
                arg[n++].reduce2ast( props );
        }
        ps.println( ") /* " + className() + " */" );
    }
}
