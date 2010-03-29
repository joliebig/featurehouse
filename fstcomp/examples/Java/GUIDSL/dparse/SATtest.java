// A SATtest object defines a model test
// each test has a name, an outcome (satifiable or not)
// plus the list of feature selections
// this information (along with the model predicate) are submitted
// to a SAT solver for verifying the outcome.

import java.util.*;

public class SATtest {
    String  name;
    boolean isSat;
     boolean completeTest;
    public Vector  selections;

    public SATtest( String testName, boolean result, boolean isComplete ) {
        name         = testName;
        isSat        = result;
        completeTest = isComplete;
        selections   = new Vector();
    }

    public void add( Object o ) {
        selections.add( o );
    }

     public boolean isComplete() { return completeTest; }

    // for debugging

    public void print() {
        System.out.println( "test (" + name + ") " + isSat );
        for ( int i = 0; i< selections.size(); i++ ) {
            String s = ( String ) selections.elementAt( i );
            System.out.println( "   " + s );
        }
        System.out.println();
    }

     public String getName() { return name; }
     public boolean getResult() { return isSat; }
     public Vector getSelections() { return selections; }
}
