

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class MethodDeclaration {
    public  MethodDeclaration overriddenBy = null;
    public String mangledName = "";

    public void ok2compose( int stage, Hashtable hb ) {
        // Step 0: do nothing if we are inside quoted text

        if ( stage != 0 ) {
            super.ok2compose( stage, hb );
            return;
        }

        // Step 1: get signature of this method and then get the corresponding
        //         method in the base

        String sig = signature();
        ClassBodyDeclaration d = ( ClassBodyDeclaration ) hb.get( sig );

        // Step 2: if the extension method does override, note this
        if ( d!=null )
 
            d.isOverridden = true;

        // Step 3: now do some modifier processing

        AST_Modifiers mods = ( AST_Modifiers ) arg[0].arg[0];
        if ( mods==null )
            return;
        boolean newPresent  = mods.findModifier( mn );
        boolean overPresent = mods.findModifier( mo );

        if ( newPresent && overPresent )
            AstNode.error( arg[2].tok[0],
                    "method " + sig + ") in refinement has both new and override modifiers" );

        // Step 4: if extension is tagged with "new", then there should
        //         be no super-method.  Remove "new" if base is a class

        if ( newPresent ) {
            if ( d!=null )
                AstNode.error( arg[2].tok[0],
                           "new method " + sig + ") in refinement is overriding "
                   + "method in base" );
            if ( kernelConstants.globals().compclass.isBaseAClass )
                mods.remModifier( mn );
        }

        // Step 5: if extension is tagged with "overrides", then there
        //         should be a super-method if base is a class.  
        //         Remove "overrides" if base is a class

        if ( overPresent ) {
            if ( d==null &&  kernelConstants.globals().compclass.isBaseAClass )
                AstNode.error( arg[2].tok[0],
                            "override method " + sig + ") in refinement does not override "
                    + "method in base" );
            if ( kernelConstants.globals().compclass.isBaseAClass )
                mods.remModifier( mo );
        }
    }
}
