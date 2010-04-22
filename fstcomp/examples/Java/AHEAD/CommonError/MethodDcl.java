

public class MethodDcl {

    static boolean seenSUPER; // uppercase
    static boolean seensuper; // lowercase

    public void checkForErrors( int stage, String file ) {

        // don't report errors if within quoted text

        if ( stage != 0 ) {
            super.checkForErrors( stage, file );
            return;
        }
                  
        // if we get here, we are about to descend into a method
        // body at the outermost level

        seenSUPER= false;
        seensuper = false;
        super.checkForErrors( stage, file );

        // here's the error: a method cannot call super and Super.
        // if Super is present, this means that the defined method
        // is a refinement.  the rule is that a method refinement
        // cannot call super (as a mixin translation won't work
        // correctly).

        if ( seenSUPER && seensuper ) {
            // arg[2].arg[0] is QName, arg[2].tok[0] is "("
            // arg[2].tok[h is used to get the line number
            QName q = ( QName ) arg[2].arg[0];
            AstTokenInterface t = arg[2].tok[0];
            AstNode.error( t, "method " + q.GetName() + 
                     " cannot both refine (Super) and invoke super");
        }
    }
}
