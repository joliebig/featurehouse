

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class RefCons {
    public void execute( int stage ) {
        if ( stage != 0 ) {
            super.executeBypass( stage );
            return;
        }
        ;

        // Step 1: form signature of constructor

        String sig = ( ( QName ) arg[0] ).GetName() + "(";
        if ( arg[1].arg[0]!=null )
            sig = sig + ( ( AST_ParList ) arg[1].arg[0] ).GetSignature();
        sig = sig + ")";

        // Step 2: create an MMOutput object with this signature.
        //         RefCons only refines

        MMOutput o = new  MMOutput().init( sig, 
                                 MMGlobals.Constructor, 
                                 MMGlobals.Refines );
        o.setlines( this.getFirstLineNum(), this.getLastLineNum() );

        // Step 3: now add o to the main object

        MMOutput m =  Main.mmresult;
        m.nested.putUnique( sig, o );
    }
}
