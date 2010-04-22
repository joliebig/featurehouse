

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class UmInterDecl {

    public void execute( int stage ) {
        MMOutput previous = null;

        if ( stage != 0 ) {
            super.execute( stage );
            return;
        };

        NamedVector nv;

        // see if we need to stack nested interface definitions
		  if (up instanceof NInterDecl) {
	        previous = Main.mmresult;
			  Main.mmresult = new MMOutput();
		  }

        MMOutput m =  Main.mmresult;
        m.setType( MMGlobals.Interface );
        m.setName( ( ( QName ) arg[0] ).GetName() );
		  if (previous != null)
		     m.setlines( getFirstLineNum(), getLastLineNum() );
		  else
           m.setlines( -1, -1 ); // entire file
		  m.setModifiers( (AstOptNode) up.arg[0] );

        if ( arg[1].arg[0] == null )
            m.setDefn( MMGlobals.Defines );
        else {
            m.setDefn( MMGlobals.Extends );
            nv = new  NamedVector( MMGlobals.Interfaces );
            arg[1].arg[0].harvestAST_QualifiedNames( nv );
            m.union( nv );

            // now do the same for super-interfaces

            nv = new  NamedVector( "super" );
            arg[1].arg[0].harvestAST_QualifiedNames( nv );
            m.union( nv );
        }

        arg[2].execute( stage );

		  // now pop stack if we are dealing with nested interfaces

		  if (previous != null) {
		     previous.nested.putUnique( m.getName(), m );
		     Main.mmresult = previous;
		  }
    }
}
