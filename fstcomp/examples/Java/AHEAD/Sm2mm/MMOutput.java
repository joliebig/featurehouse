

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class MMOutput {

    public boolean equals( Object o ) {
        // edge names uniquely have ":" in them
        // here is a test for equality of edge names
        // strip off the start->end portion and just compare the
        // names

        int k = name.indexOf( ':' );
        if ( k != -1 ) {
            if ( o instanceof  MMOutput ) {
                String shortened = name.substring( 0,k-1 );
                String oName = ( ( MMOutput ) o ).name;
                int j = oName.indexOf( ':' );
                if ( j != -1 )
                    return shortened.equals( oName.substring( 0,j-1 ) );
                else
                    return false;
            }
            return false;
        }
        return original( o );
    }
}
