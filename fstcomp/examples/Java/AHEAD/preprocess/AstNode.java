

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

/********************* Kernel Classes *********************
 * @layer<preprocess>
 */

public abstract class AstNode {

    // tag all AstNodes with the name of their source layer
    // once labeled with a source, it is not overridden.

    public String _source = null;

    public void setSource( String s ) {
        int i;
        if ( _source == null )
            _source = s;
        if ( arg == null )
            return;
        for ( i=0; i<arg.length; i++ )
            if ( arg[i]!=null )
                arg[i].setSource( s );
    }

    public String getSource() {
        return _source;
    }

    public void compose( AstNode etree ) {
        if ( arg == null )
            return;
        for ( int i=0; i<arg.length; i++ )
            if ( arg[i]!=null )
                arg[i].compose( etree.arg[i] );
    }

}
