

import java.util.*;
import java.io.*;

public class OtherwiseClauses {

    public void compose( AstNode etree ) {
        // Step 1: composition of otherwise clauses is simple.  just append
        //         the list of the extension to the list of the base

        add( ( OtherwiseClauses ) etree );
    }

    public void harvestAst( LinkedList ll ) {
        // foreach element on an OtherwiseClauses list, invoke harvestAst

        AstCursor c = new  AstCursor();

        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            ( ( ODefaultDecl ) c.node ).harvestAst( ll );
        }
    }
}
