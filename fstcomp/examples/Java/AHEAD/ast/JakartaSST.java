

import java.io.PrintWriter;

// this is a major simplification of the original ast layer
// most actions for code constructors is the same.  These methods
// are placed in JakartaSST

//**************************************************
// JakartaSST
//**************************************************
    
public class JakartaSST {
    private int stackMarker;
 
    public void reduce2java( AstProperties props ) {
        boolean order[];
        int i, t, n;
        PrintWriter ps;
        Environment env;
        AstNode a0 = arg[0];

        ps = ( PrintWriter ) props.getProperty( "output" );

             // conditional test -- this is the only difference
             // between differnt code constructors.

        // return null if the lone code constructor is an AstOptNode
                  // and it has a null argument -- this is the translation for empty
                  // AST constructors.

        if ( a0 instanceof  AstOptNode ) {
            a0 = a0.arg[0];
            if ( a0 == null ) {
                ps.print( " null" );
                return;
            }
        }

        env = ( Environment ) props.getProperty( "env" );
        if ( env != null )
            env.setActive( true );

        // Start passing a level count marker for AST's

        Integer oldLevel = ( Integer ) props.getProperty( "AstLevel" );
        if ( oldLevel == null )
            props.setProperty( "AstLevel", new Integer( 1 ) );
        else
            props.setProperty( "AstLevel", new Integer( oldLevel.intValue()+1 ) );

        // now reduce to AST the lone argument.  Remember the aliasStacksize

        String qual =  kernelConstants.LangName;
        String name = className();
        String nam = name.substring( 0, name.length()-1 );
        ps.print( "(" + nam + ") " + qual + 
                                                                "AstNode.markStack(" + qual + 
                                                                "AstNode.aliasStack.size(), " );
        a0.reduce2ast( props );

        // finish off with a call to patch

        ps.println( ").patch()" );
        if ( env != null )
            env.setActive( false );

        // Reduce or remove AstLevel
        if ( oldLevel == null )
            props.removeProperty( "AstLevel" );
        else
            props.setProperty( "AstLevel", new Integer( oldLevel.intValue()-1 ) );
    }

    public void reduce2ast( AstProperties props ) {
        Integer oldLevel;

        // Increment AstLevel
        oldLevel = ( Integer ) props.getProperty( "AstLevel" );
        props.setProperty( "AstLevel", new Integer( oldLevel.intValue()+1 ) );

        super.reduce2ast( props );

        // Restore old level (decrement)
        props.setProperty( "AstLevel", oldLevel );
    }

    public void buildSymbolTable1() {}

    public void buildSymbolTable2() {}
}
