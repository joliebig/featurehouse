layer requireBali2javacc;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 * Extends with methods to list rules that have various unusual conditions.
 *
 * @layer<requireBali2javacc>
 */
    
public refines class BaliRulesData {

    /**
     * Lists those rules that appear in "require" statements, but are
     * neither defined nor referenced.
     *
     * @layer<requireBali2javacc>
     */
    public List misRequired() {
        return getList( DEFINED | REFERENCED | REQUIRED, REQUIRED ) ;
    }

    /**
     * Lists those rules that are defined ("require" statements don't
     * count), but not referenced.
     *
     * @layer<requireBali2javacc>
     */
    public List unReferenced() {
        return getList( DEFINED | REFERENCED, DEFINED ) ;
    }

}
