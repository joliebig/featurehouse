

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class MethodDcl {

    /** mangled names have the format "<name>$<mangled-identifier"
     *  this method returns <name>
     * @layer<sortfd>
     */
    String originalName( String arg ) {
        int pos = arg.indexOf( "$$" );
        if ( pos >= 0 )
            return arg.substring( 0,pos );
        else
            return arg;
    }

    public String GetName() {
        return ( ( MethodDeclarator ) arg[2] ).GetName();
    }

    public String GetUnmangledName() {
        return originalName( GetName() );
    }
}
