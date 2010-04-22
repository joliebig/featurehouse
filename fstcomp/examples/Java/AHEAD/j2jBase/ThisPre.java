

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class ThisPre {
    public void reduce2java( AstProperties props ) {

        // Step 1: reduce "this" construct to "this" (normal reduction)
		  //         if SoUrCe not seen or if we are inside a constructor

        Object o = props.getProperty( "insideConstructor" );
		  if ( props.getProperty( "SoUrCe" ) == null || o != null ) {
		      super.reduce2java( props );
				return;
		  }

        // Step 2: else we've seen a SoUrCe tag.  This means
		  //         that "this" is rewritten to "( (<classname>) this)"

        String ThisName = (String) props.getProperty( "ThisName" );
		  if (ThisName == null)
		     AstNode.fatalError( "ThisName property not set" );
		  props.print( getComment() + "((" + ThisName + ") this)");
    }
}
