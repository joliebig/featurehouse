

import java.util.*;
import java.io.*;
import Jakarta.util.*;

public class UmodClassDecl    {

    String previous = "";

    public void reduce2java( AstProperties props ) {

        // Step 1: remember the name of the class that is being extended
        //         this is needed for correctly expanding Super() constructs
        //         inside static methods.

        String extendsName = "";
        ExtendsClause ec = ( ExtendsClause ) arg[1].arg[0];
        if ( ec != null )
            extendsName = ec.GetName();
        props.setProperty( "SuperName", extendsName );
        String className = arg[0].tok[0].getTokenName();
		  previous = (String) props.getProperty( "ThisName" );
		  if (previous == null)
		     previous = "";
		  props.setProperty( "ThisName", className );

        // Step 2: do a normal reduction and return

        super.reduce2java( props );
        props.setProperty( "MixinDeclName", "" );
        props.setProperty( "SuperName", "" );
		  props.setProperty( "ThisName", previous );
    }
}
