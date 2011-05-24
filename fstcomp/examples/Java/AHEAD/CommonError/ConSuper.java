

public class ConSuper {

    public void checkForErrors( int stage, String file ) {

        // don't report errors if within quoted text

        if ( stage != 0 ) {
            super.checkForErrors( stage, file );
            return;
        }
                  
        // if we get here, we are within a constructor whose
        // body contains a super(...) call.  If we are within
        // a class refinement, then this is an error

        if (Ute.withinRefines) {
            AstNode.error( tok[0], 
                "constructor within a class refinement cannot use super");
        }

        // now check the rest
           
        super.checkForErrors( stage, file );
    }
}
