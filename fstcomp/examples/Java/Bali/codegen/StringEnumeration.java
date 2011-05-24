

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

//-----------------------------------------------------------------------//
// String enumerations for building token/keyword enumerations:
//-----------------------------------------------------------------------//

public class StringEnumeration implements Comparable {

    public int compareTo( Object that ) {
        return token.compareTo( ( ( StringEnumeration ) that ) . token ) ;
    }

    public boolean equals( Object object ) {
        return ( this == object ) ;
    }

    public String toString() {
        return token ;
    }

    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    protected StringEnumeration( String token ) {
        this.token = token.intern() ;
    }

    final private String token ;

}
