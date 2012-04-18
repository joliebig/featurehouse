

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;

//-----------------------------------------------------------------------//
// Translator-specific Collector class:
//-----------------------------------------------------------------------//

public class Collector {

    public void generateNonterminals( File directory ) {
        Main.DEBUG.entering( "bali2jak.Collector","generateNonterminals" ) ;
        baliRules.generateClasses( directory, layerName ) ;
        Main.DEBUG.exiting( "bali2jak.Collector","generateNonterminals" ) ;
    }

    public void setLayer( String layerName ) {
        this.layerName = layerName ;
    }

    private String layerName = null ;

}
