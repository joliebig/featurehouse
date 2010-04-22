

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class UmodClassExt {

    public String GetName() {
        return arg[0].tok[0].tokenName();
    }
    public String GetType() {
        return "class";
    }

    public void compose( AstNode etree ) {
        // composition of class declarations involves three steps.
        // (a) compose implements clause
        // (b) compose bodies

        // Step 0: set flag for error checking

        kernelConstants.globals().compclass.isBaseAClass = false;

        // Step 1: first, check if composition is possible

        UmodClassExt e = ( UmodClassExt ) etree;
        String bname = arg[0].tok[0].tokenName();
        String ename = e.arg[0].tok[0].tokenName();
        if ( !bname.equals( ename ) )
            AstNode.fatalError( tok[0],
                        "attempting to compose files with different names: " +
                    bname + "  " + ename );

        // Step 2: compose implements clauses, and then classbodies

        arg[1].compose( e.arg[1] );
        arg[2].compose( e.arg[2] );
    }
}
