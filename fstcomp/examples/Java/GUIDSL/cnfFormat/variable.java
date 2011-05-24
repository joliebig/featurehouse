import Jakarta.util.*;
import java.util.*;
import java.io.*;

class variable {
    int number = -1;

    public static variable define( String name, int type, gObj g, boolean redefinable ) {
        variable result = original( name, type, g, redefinable );
        if ( result.number == -1 ) {
            result.number = variable.vtsize;
                       // for debugging cnf files
        // System.out.println(result.number + " " +result.name);
        }
        return result;
    }

    static variable find( String name ) {
        return ( variable ) Vtable.get( name );
    }

    static int findNumber( String name ) throws dparseException {
        variable v = find( name );
        if ( v != null )
            return v.number;
        throw new dparseException( "unrecognizable variable: " + name );
    }

    static String findVar( int num ) {
        Iterator i = Vtable.values().iterator();
        while ( i.hasNext() ) {
            variable v = ( variable ) i.next();
            if ( v.number == num )
                return v.name;
        }
        return null;
    }

    // for debugging cnf files
    static void dumpVariablesInOrder( PrintWriter pw ) {
        try {
            for ( int i=0; i<Vtable.size(); i++ ) {
                int v = i+1;
                pw.println( "c c " + v + " " + findVar( v ) );
            }
        }
        catch ( Exception e ) {
            Util.fatalError( "dumpVariablesInOrder Exception " +
                                                        e.getMessage() );
        }
    }

}
