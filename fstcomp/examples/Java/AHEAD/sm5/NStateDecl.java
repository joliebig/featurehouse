

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class NStateDecl {
    public void reduce2java( AstProperties props ) {
        String           stateName;
        stateInfo        s;

        // Step 1: harvest state information

        stateName = arg[0].tok[0].tokenName();
        s =  StatesClause.defineState( stateName, true, tok[0] );
        s.alloc_expr_ast = arg[1];

        // Step 2: check if expression is of the correct type

        if ( ! ( s.alloc_expr_ast instanceof  ObjAllocExpr ) )
            AstNode.fatalError( tok[0], Utility.SourceName() +
                           " illegal constructor for nested state " 
                           + stateName );
    }
}
