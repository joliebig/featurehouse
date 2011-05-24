

import java.io.PrintWriter;

public class AST_QualifiedName {
    // makeQName finds the first QName in an AST_QualifiedName,
    // and returns a clone of it.  This method is needed for nameid 
    // escapes -- such an escape takes an AST_QualifiedName as input
    // but must return a QName (or name id) as output.

    public  QName makeQName() {
        AstCursor c = new  AstCursor();
        for ( c.First( this ); c.More(); c.PlusPlus() ) {
            if ( c.node instanceof  QName ) {
                return ( QName ) c.node.clone();
            }
        }
        AstNode.fatalError( "No QName found in AST_QualifiedName" );
        return null; // will never get here
    }
}
