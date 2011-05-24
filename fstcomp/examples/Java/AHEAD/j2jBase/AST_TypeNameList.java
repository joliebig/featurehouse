

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class AST_TypeNameList {

    // this returns a vector of strings that correspond to a list
    // of type names.  It is used in manufacturing the string representation
    // of a constructor

    public Vector extractTypes() {
        Vector v  = new Vector();

        AstCursor c = new  AstCursor();
        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            TName t = ( TName ) c.node;
            v.add( t.GetName() );
        }
        return v;
    }

    public String formSignature() {
        return makeSignature( extractTypes() );
    }

    // makeSignature is really a utility that can be called from anywhere...

    static String makeSignature( Vector v ) {
        int size = v.size();
        String sig = "";

        // Step 2: for each type, concatenate it

        for ( int i=0; i<size; i++ ) {
            if ( i>0 )
                sig = sig + ",";
            sig = sig + ( ( String ) v.elementAt( i ) );
        }

        return sig;
    }

    public String formConstructor() {
        int i;

        // Step 1: extract the type names

        Vector v = extractTypes();
        int size = v.size();
        String k = "      clsname( "; // actual name will be supplied later

        // Step 2: for each type name, add a formal parameter

        for ( i=0; i<size; i++ ) {
            if ( i>0 )
                k = k + ", ";
            k = k + ( ( String ) v.elementAt( i ) ) + " v" + i;
        }

        // Step 3: finish off decl and prepare for contents

        k = k + ") { super( ";

        // Step 4: for each type name, add its parameter

        for ( i=0; i<size; i++ ) {
            if ( i>0 )
                k = k + ", ";
            k = k + " v" + i;
        }

        // Step 5: finish constructor and return

        return k + "); }";
    }
}
