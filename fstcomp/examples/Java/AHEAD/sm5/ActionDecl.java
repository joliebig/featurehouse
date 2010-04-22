

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class ActionDecl {
    public void reduce2java( AstProperties props ) {
        String    transitionName;
        transInfo e;

        transitionName = arg[0].tok[0].tokenName();
        e =  transInfo.verifyTransName( transitionName, "Action method", tok[0] );
        AST_Stmt code = ( AST_Stmt ) arg[1].arg[0].arg[0];

        // now do the refinement -- there can be multiple Action declarations
        // in an SM specification.  This wierdness arises because of JamPack
        // compositions of SM specs.

        e.action_ast = ( AST_Stmt )
          kernelConstants.globals().sm4vars.refineMethod( e.action_ast, code, "Transition_action " +
                    transitionName, e.name + "_action", true, tok[0] );
    }
}
