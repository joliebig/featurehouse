

import java.io.PrintWriter;

//**************************************************
// Env extension class
//**************************************************
    
public class Env {
    public void reduce2java( AstProperties props ) {
        AstCursor csr;
        AstNode ancestor;
        PrintWriter ps;
        Environment env;
        AstTokenInterface token;
        String tname;

        ps = ( PrintWriter ) props.getProperty( "output" );
        ancestor = hasAncestor( "ClsBody" );
        if ( ancestor != null ) {
            // context is class body

            // Create an environment owned by the class
            env = new  Environment();
            props.setProperty( "env", env );

            // Generate code for creating a new environment
            ps.print( "\n\tpublic " +  kernelConstants.LangName + "Environment _E = (" +
                                      kernelConstants.LangName + "Environment) new " +  kernelConstants.LangName +
                                     "Environment()" );
            csr = new  AstCursor();
            csr.First( arg[0] );
            csr.PlusPlus();
            AstToken.printWhitespace( false );
            while ( csr.More() ) {
                tname = csr.node.tok[0].tokenName();

                if ( tname.length() > 0 ) {
                    // Add any identifiers names to the environment
                    env.addId( tname );

                    ps.print( ".addId(\"" );
                    csr.reduce2java( props );
                    ps.print( "\")" );
                }
                csr.PlusPlus();
            }
            AstToken.printWhitespace( true );
            ps.println( ";\n\tpublic " +  kernelConstants.LangName +
                                       "Environment getEnv() { return(_E); }" );

            return;
        }

        ancestor = hasAncestor( "FDecl" );
        if ( ancestor != null ) {
            // context is interface body
            ps.println( "\t" +  kernelConstants.LangName + "Environment getEnv();" );
        }
        else
            AstNode.fatalError( tok[0],
                             "Can't find context for 'environment' decl" );
    }
}
