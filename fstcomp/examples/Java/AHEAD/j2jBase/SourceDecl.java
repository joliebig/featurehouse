

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

//----------  mixin-layer for SourceDecl ----------------

public class SourceDecl {

    public void harvestConstructors( int stage ) {
        // just set the SourceDeclSeen boolean; doing so will
        // allow constructor propagation errors to be reported

        // do nothing if we are within quoted text

        if ( stage != 0 ) {
            super.harvestConstructors( stage );
            return;
        }

        kernelConstants.globals().j2jbase.SourceDeclSeen = true;
    }

    public void reduce2java( AstProperties props ) {
        // do nothing -- this statement vanishes in translation to java
        // although do note that this statement has been seen.  
        // just declare the property SoUrCe exists; that's all we need
        // do, however, print the white-space in front for pretty-printing
        // Translations of other statements are conditional based
        // on the presense or absense of source statements

        // some error checking first.  If a SoUrCe statement is seen
        // by jak2java, then the first one must declare a ROOT token
        // otherwise jak2java is translating a refinement (function)
        // not a class (constant)

        if ( props.getProperty( "SoUrCe" ) == null ) {
            if ( tok[1].tokenName().equals( "" ) )
                AstNode.error( tok[0], 
                               "cannot translate a refinement, only base classes" );
        }
        props.print( getComment() );
        props.setProperty( "SoUrCe", "non-Null" );
        kernelConstants.globals().j2jbase.SourceDeclCounter++;
    }
}
