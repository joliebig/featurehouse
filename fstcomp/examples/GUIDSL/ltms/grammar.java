import java.util.*;
import javax.swing.JComponent;

class grammar {

    // this is the set of selections that are to be propagated by an LTMS
    static ArrayList UserSelections = new ArrayList();

    private static boolean debugIsOn = false;
    private static boolean updateNow = true;
    private static boolean beginning = true;

    static variable root = null;

    static variable getRoot() {
       if (root != null) return root;
       String rootName = ( grammar.rootProduction ).name;
       root = ( variable ) ( variable.Vtable.get( rootName ) );
       return root;
    }
   
    static void initDebugTable()    // initialize debug table
   {
        if ( beginning ) {
            reset();
        }

        DebugTable.createAndShowGUI();
        debugIsOn = true;
    }

    static void initFormulas()      // initialize propagation formulas table
   {
        Formulas.createAndShowGUI();
    }

    static void propagate()      // clear variables, and propagate UserSelections
   {
/* DEBUG
System.out.println( " ----------------propagate() called --------------- " );
*/
        beginning = false;

        updateNow = false;
        reset();
        updateNow = true;

        cnfClause.stack = new Stack();

        // always set the root
        variable root = getRoot();
        root.resetRoot(); 
        root.set(false);
        cnfClause.BCP();

        Iterator i = UserSelections.iterator();
        while ( i.hasNext() ) {
            variable v = ( variable ) i.next();
            v.set(false);   // set variable to be true
            v.justify();    // set by user
            cnfClause.BCP();
        }
/* DEBUG
System.out.println("----- Propagate done ----");
*/

        if ( debugIsOn )
        { // update debug table components
            for ( int row = 0; row < DebugTable.sortedVtable.length; row++ )
            {
                variable v = ( variable ) ( variable.Vtable.get( DebugTable.sortedVtable[row] ) );
                if ( v.value == variable.T ) 
                                                                                 {
                    DebugTable.data[row][1] = "True";
                }
                else
                    if ( v.value == variable.F )
                {
                        DebugTable.data[row][1] = "False";
                    }
                    else    // v.value == variable.U
                {
                        DebugTable.data[row][1] = "Unknown";
                    }

                if ( v.userSet )
                {
                    DebugTable.data[row][2] = "True";
                }
                else
                {
                    DebugTable.data[row][2] = "False";
                }
            }
            DebugTable.update();
        }
    }

    public static void reset()     // reset variables
   {
        Iterator vars = ( variable.Vtable.values() ).iterator();
        while( vars.hasNext() )
        {
            variable v = ( variable ) ( vars.next() );
            v.reset();
/* DEBUG
            v.value = variable.U;
            v.userSet = false;
            v.explanation = "";
*/
        }
        variable root = getRoot();
        root.resetRoot(); 
/* DEBUG
        root.value = variable.T;
        root.userSet = true;
        root.explanation = "Root";
*/

/* DEBUG

        if( debugIsOn && updateNow )
        { // update debug table components
            for( int row = 0; row < DebugTable.sortedVtable.length; row++ )
            {
                variable v = ( variable ) ( variable.Vtable.get( DebugTable.sortedVtable[row] ) );
                if( rootName.equals( v.name ) )
                {
                    DebugTable.data[row][1] = "True";
                    DebugTable.data[row][2] = "False";
                }
                else
                {
                    DebugTable.data[row][1] = "Unknown";
                    DebugTable.data[row][2] = "True";
                }
            }
            DebugTable.update();
        }
*/
    }
}
