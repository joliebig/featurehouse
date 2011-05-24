

import java.io.PrintWriter;

//**************************************************
// ClsIscape extension class
//**************************************************
    
public class ClsIscape {
    public  AstNode myAlias;

    // This method gets called if the Iscape is in a top-level
    // constructor.
    public  AstNode setParms( Environment env,  AstTokenInterface _arg0 ) {

        setParms( ( AstToken ) _arg0 );
        EnvElem elem = ( EnvElem ) env.findId( tok[0].tokenName() );
        if ( elem == null ) {
            // Error. Must have an entry to support aliasing.
            AstNode.fatalError( tok[0], 
                               "No environment for implicit class escape (" +
                               tok[0].tokenName() + ")" );
        }

        if ( elem._alias == null ) {
            // Error. No alias defined for this identifier.
            AstNode.fatalError( tok[0],
                                "No alias for implicit class escape (" +
                                    tok[0].tokenName() + ")" );
        }

        aliasStack.push( this );
        myAlias = elem._alias;
        return ( ClsIscape ) this ;
    }

    public void doPatch() {
        AstNode alias_ast;

        if ( ! ( myAlias instanceof AST_Class ) ) {
            System.err.println( "Didn't do patch - alias is " +
                                                   myAlias.getClass().getName() );
            System.err.println( "AST_Class required for alias.\n" );
            return;
        }

        alias_ast = ( AstNode ) myAlias.clone();
        this.Replace( alias_ast );
    }

    public void reduce2ast( AstProperties props ) {
        PrintWriter ps;
        Integer level;

        ps = ( PrintWriter ) props.getProperty( "output" );
        if ( props.getProperty( "env" ) == null ) {
            // Error. No env indicates we are either not in an AST
            // constructor or the one we are in doesn't have an
            // environment declaration in it.
            AstNode.fatalError( tok[0],
                              "Implicit class escape must be in constructor!" );
        }

        // Get level number
        level = ( Integer ) props.getProperty( "AstLevel" );

        ps.print( " (" + className() + ") new " + className() +
                     "().setParms(" );
        if ( level.intValue() == 1 )
            ps.print( "_E, " );

        tok[0].reduce2ast( props );
        //             ps.print(", ");
        //             tok[1].reduce2ast(props);
        ps.println( ") /* " + className() + " */" );
    }
}
