

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class EnterDecl {
    public void reduce2java( AstProperties props ) {
        String    stateName;
        stateInfo s;
        AST_Stmt code;

        stateName = arg[0].tok[0].tokenName();
        s =  stateInfo.verifyStateName( stateName, "Enter method", tok[0] );

        code = ( AST_Stmt ) arg[1].arg[0].arg[0];

        // now do the refinement -- there can be multiple Enter declarations
        // in an SM specification.  This wierdness arises because of JamPack
        // compositions of SM specs.

        s.enter_action_ast = ( AST_Stmt )
          kernelConstants.globals().sm4vars.refineMethod( s.enter_action_ast, code, "Enter " + stateName, 
              s.name + "_enter_user", true, tok[0] );
    }
}
