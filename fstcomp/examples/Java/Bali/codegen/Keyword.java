

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

public class Keyword extends  StringEnumeration {

    final public static  Keyword
        CLASS = new  Keyword( "class" ),
        EXTENDS = new  Keyword( "extends" ),
        IMPORT = new  Keyword( "import" ),
        IMPLEMENTS = new  Keyword( "implements" ),
        INTERFACE = new  Keyword( "interface" ),
        LAYER = new  Keyword( "layer" ),
        RETURN = new  Keyword( "return" ) ;

    protected Keyword( String token ) {
            super( token ) ;
        ;
    }

}
