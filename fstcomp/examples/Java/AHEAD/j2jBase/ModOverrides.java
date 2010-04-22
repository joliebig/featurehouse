

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

//------------------------ j2jBase layer -------------------
//------ mixin-layer for dealing with Overrides and New Modifier

//------ offhand, I wonder why we can't let j2j map these modifiers
//------ to nothing, instead of letting PJ do some of this mapping
//       it would make more sense for PJ and Mixin to have similar
//       output.

public class ModOverrides {

    public void reduce2java( AstProperties props ) {
        // Step 1: the overrides modifier should not be present, UNLESS
        //         there is a SoUrCe property.  It's an error otherwise.

        if ( props.getProperty( "SoUrCe" ) == null ) {
            AstNode.error( tok[0], "overrides modifier should not be present" );
            return;
        }
       
        // Step 2: it's OK to be present.  If so, the reduction is to 
        //         print the white-space in front for pretty-printing

        props.print( getComment() );
    }
}
