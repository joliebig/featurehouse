

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class UmodClassDecl      {

    public void execute( int stage ) {
	     MMOutput previous = null;

        if ( stage != 0 ) {
            super.execute( stage );
            return;
        }
        ;

        NamedVector nv;

		  // see if we need to stack nested class definitions
		  if (up instanceof NestedClassDeclaration) {
		     previous = Main.mmresult;
			  Main.mmresult = new MMOutput();
		  }

        MMOutput m =  Main.mmresult;
        m.setType( MMGlobals.Class );
        m.setName( ( ( QName ) arg[0] ).GetName() );
		  if (previous == null)
		     m.setlines( -1, -1 ); // entire file
		  else
           m.setlines( getFirstLineNum(), getLastLineNum() );
		  m.setModifiers( (AstOptNode) up.arg[0] );

        // do the extends clause first

        if ( arg[1].arg[0] == null )
            m.setDefn( MMGlobals.Defines );
        else {
            m.setDefn( MMGlobals.Extends );
            nv = new  NamedVector( MMGlobals.Classes );
            arg[1].arg[0].harvestAST_QualifiedNames( nv );
            m.union( nv );

            // remember the names of the super class

            nv = new  NamedVector( "super" );
            arg[1].arg[0].harvestAST_QualifiedNames( nv );
            m.union( nv );

        }

        // now do the implements claus         

        if ( arg[2].arg[0] != null ) {
            nv = new  NamedVector( MMGlobals.Interfaces );
            arg[2].arg[0].harvestAST_QualifiedNames( nv );
            m.union( nv );
        }

        // now harvest the field members and methods, and we're done

        arg[3].execute( stage );

		  // now pop stack if we were dealing with nested classes
		  if (previous != null) {
		     previous.nested.putUnique( m.getName(), m );
		     Main.mmresult = previous;
		  }
    }
}
