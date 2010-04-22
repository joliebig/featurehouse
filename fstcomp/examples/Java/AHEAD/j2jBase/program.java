

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class program {

    public void reduce2java( AstProperties props ) {

        // Step 1: harvest constructors in the first-pass
        //         really only useful when translating Mixin-produced files
        //         the SourceDeclCounter counts the number of SoUrCe
        //         declarations in this file.

        j2jBasedata j2jbase =  kernelConstants.globals().j2jbase;
        j2jbase.inheritedCons     = new  conTable();
        j2jbase.previousTypeDecls = new Vector();
        j2jbase.SourceDeclCounter = 0;
        j2jbase.SourceDeclSeen    = false;
        j2jbase.constructorTable  = new Hashtable();
        harvestConstructors( 0 );

        // Step 2: reduce normally in second pass

        super.reduce2java( props );
    }
}
