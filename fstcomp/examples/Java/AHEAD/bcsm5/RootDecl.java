

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class RootDecl {
   // define start and stop states as inherited

	void defineStates( boolean define ) {
	   if (!define) { // pretend that start and stop are inherited
         stateInfo s;
         s = new stateInfo( "start", false );
         s.inherited = true;
         kernelConstants.globals().sm4vars.Sm.StateCont.add( s );

         s = new stateInfo( "stop", false );
         s.inherited = true;
         kernelConstants.globals().sm4vars.Sm.StateCont.add( s );
      }
		else  // add them as legitimate states
		   original(define);
	}
}
