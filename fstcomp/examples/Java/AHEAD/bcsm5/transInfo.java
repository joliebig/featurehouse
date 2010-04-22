

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// transInfo is instantiated per transition declaration.

class transInfo {

    // override original definition by extending the method so
	 // that if the edge isn't found, it is declared.

    static  transInfo verifyTransName( String ename, String which, 
                                               AstTokenInterface t ) {
        transInfo e;

        SmIterator i =  kernelConstants.globals().sm4vars.Sm.TransCont.iterator();
        for ( e = ( transInfo ) i.firstObj(); 
               e != null; 
               e = ( transInfo ) i.nextObj() ) {
            if ( e.name.equals( ename ) )
                return e;
        }

        // if we can't find it, we'll declare a dummy edge and
		  // add it to the edge container

		  e = new transInfo( ename, "%%UNKNOWN%%", "%%UNKNOWN" );
		  e.inherited = true;
        kernelConstants.globals().sm4vars.Sm.TransCont.add( e );

		  return e;
    }

    // since we don't serialize anything, we don't need to truncate
	 public void truncate() { }
}
