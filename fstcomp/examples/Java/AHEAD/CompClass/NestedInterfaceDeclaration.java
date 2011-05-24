

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class NestedInterfaceDeclaration {
    public void ok2compose( int stage, Hashtable hb ) {

        // do nothing if we are inside quoted text else issue warning

        if ( stage != 0 )
            return;
        AstNode.warning( arg[0].tok[0],
               "CompClass cannot compose nested interfaces" );
    }

    public String signature() {
        return "Interface-" +  kernelConstants.globals().compclass.Initializer_counter++;
    }
}
