

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class BasePre {

    public void execute( int stage ) {
        if ( stage != 0 ) {
            super.execute( stage );
            return;
        }
        ;

        // Step 1: get the signature of the super method

        String sig = "()";
        if ( arg[0].arg[0] != null )
            sig = ( ( AST_TypeNameList ) arg[0].arg[0] ).GetSignature();
        sig = ( ( QName ) arg[1] ).GetName() + sig;
         
        // Step 2: if super signature equals parentSignature do a quick return
        //         to be caught by MethodDcl.execute().  Else, do nothing

        if ( sig.equals( parentSig ) )
            throw new ResultException();
        return;
    }
}
