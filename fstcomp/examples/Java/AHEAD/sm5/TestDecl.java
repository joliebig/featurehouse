

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class TestDecl {
    public void reduce2java( AstProperties props ) {
        String    transitionName;
        transInfo e;

        transitionName = arg[0].tok[0].tokenName();
        e =  transInfo.verifyTransName( transitionName, "Test method", tok[0] );
        AST_Exp code = ( AST_Exp ) arg[1];

        // now do the refinement -- there can be multiple Test declarations
        // in an SM specification.  This wierdness arises because of JamPack
        // compositions of SM specs.

        e.condition_ast = ( AST_Exp )
          kernelConstants.globals().sm4vars.refineMethod( e.condition_ast, code, "Transition_test " +
              transitionName, e.name + "_test", false, tok[0] );

    }
}
