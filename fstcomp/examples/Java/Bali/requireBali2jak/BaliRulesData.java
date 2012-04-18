

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Inhibit generation of unwanted parse-tree classes.
 *
 * @layer<requireBali2jak>
 */
    
public class BaliRulesData {

    /**
     * Inhibit generation of unwanted parse-tree classes.  Currently,
     * referenced but undefined rules won't get converted to classes, nor
     * will rules named in "require" statements.
     *
     * @layer<requireBali2jak>
     */
    protected void initializeGeneratedSet() {
        generatedSet.clear() ;
        generatedSet.addAll( getList( DEFINED | REFERENCED, REFERENCED ) ) ;
        generatedSet.addAll( getList( REQUIRED, REQUIRED ) ) ;
    }

}
