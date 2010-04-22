

public class Ute { 

    static boolean withinRefines;

    public void checkForErrors( int stage, String file ) {

        // don't report errors if within quoted text

        if ( stage != 0 ) {
            super.checkForErrors( stage, file );
            return;
        }
		  
	// if we get here, we are about to descend into a refinement 
	// at the outermost level

	withinRefines = true;
        super.checkForErrors( stage, file );
        withinRefines = false;
    }
}
