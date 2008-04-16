import java.util.*;
import javax.swing.*;

class variable {
    boolean userSet = false; // true if set by user, false otherwise

    public void reset() {
      userSet = false;
      value = variable.U;
      // to be extended by later layers
    }

    void resetRoot() {
      userSet = true;
      value = variable.T;
    }

    // returns justification for variable's value
    // this method is actually "abstract", and whose body will
    // be supplied by lower layers

    String explainValue() { return ""; }

    public void print() {
        original();
        System.out.print( "userSet is " + userSet );
    }

    public void justify( cnfClause reason ) { /* filled in by later layers */ }

    public void justify( ) {
       userSet = true;
       // extended by later layers
    }

    public void set(boolean negated) {
        // set value of variable to be true

        if (negated)
           value = variable.F;
        else
           value = variable.T;

        // for each clause in clist that contains not this-term

        Iterator i = cnfClause.clist.iterator();
        while ( i.hasNext() ) {
            cnfClause c = ( cnfClause ) i.next();
            if ( c.hasNegTerm( !negated, ( variable )this ) ) {
                if ( c.isUnitOpen() != null )
                    cnfClause.stack.push( c );
                else
                    if ( c.isViolated() ) {
                        JOptionPane.showMessageDialog( null,
                        "model inconsistency detected",
                        "Error!", JOptionPane.ERROR_MESSAGE );
                        System.exit(1);
                    }

            }
        }
    }
}
