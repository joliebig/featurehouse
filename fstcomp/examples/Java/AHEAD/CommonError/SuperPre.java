

public class SuperPre {

    public void checkForErrors( int stage, String file ) {

        // don't report errors if "super.xxx" appears in quoted text

        if ( stage != 0 ) {
            super.checkForErrors( stage, file );
            return;
        }

		  MethodDcl.seensuper = true;
    }
}
