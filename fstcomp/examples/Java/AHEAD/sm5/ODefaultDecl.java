

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class ODefaultDecl {
    public void reduce2java( AstProperties props ) {
        AST_Stmt code = ( AST_Stmt ) arg[0].arg[0].arg[0];

        // now do the refinement -- there can be multiple o-d declarations
        // in an SM specification.  This wierdness arises because of JamPack
        // compositions of SM specs.

        kernelConstants.globals().sm4vars.Sm.otherwise_default_ast = ( AST_Stmt )
          kernelConstants.globals().sm4vars.refineMethod( kernelConstants.globals().sm4vars.Sm.otherwise_default_ast, code, 
             "Otherwise_default",  "otherwise_Default", true, tok[0] );

    }
}
