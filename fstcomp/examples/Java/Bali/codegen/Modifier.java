

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

/**
 * Base class for type-safe enumeration of type modifiers for generated
 * source code.
 *
 * @layer<codegen>
 */
    
public class Modifier extends  StringEnumeration {

    final public static  Modifier
        ABSTRACT = new  Modifier( "abstract" ),
        PRIVATE = new  Modifier( "private" ),
        PUBLIC = new  Modifier( "public" ),
        REFINES = new  Modifier( "refines" ),
        STATIC = new  Modifier( "static" ) ;

    protected Modifier( String token ) {
            super( token ) ;
        ;
    }

}
