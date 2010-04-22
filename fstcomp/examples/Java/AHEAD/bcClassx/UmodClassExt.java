

import java.util.*;
import java.io.*;

public class UmodClassExt   {

    // (refines) class qname implements classbody reduces to
	 // (refines) class qname implements extends stub.qname classbody

    public void reduce2java( AstProperties props ) {
	     // Step 1: set SuperName (the name of the assumed superclass
	     String className = arg[0].tok[0].tokenName();
		  props.setProperty( "SuperName", className );
		  props.setProperty( "ThisName", className );

        // Step 2: and now the reduction

		  tok[0].print( props );      // class
		  arg[0].reduce2java(props);  // Qname
		  props.print(" extends " + kernelConstants.stub + "." + className + " ");
		  arg[1].reduce2java(props);  // implements
		  arg[2].reduce2java(props);  // body

		  // Step 3: remove SuperName property
		  props.removeProperty( "SuperName" );
		  props.removeProperty( "ThisName" );
    }
}
