

import java.util.*;
import Jakarta.util.*;
import java.io.*;

/********************* Kernel Classes *********************
 * @layer<mixinbase>
 */

public abstract class AstNode {

    // tag all AstNodes with the name of their source layer

    public String _source = null;

    public void setSource( String s ) {
        int i;
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

    // common routines for setting and getting tokenNames

    public String argTokenName( int argNumber ) {
        return arg[argNumber].tokenName( 0 );
    }

    public void setArgTokenName( int argNumber, String value ) {
        arg[argNumber].setTokenName( 0,value );
    }

    public String tokenName( int tokNumber ) {
        return tok[tokNumber].tokenName();
    }

    public void setTokenName( int tokNumber, String value ) {
        ( ( AstToken ) tok[tokNumber] ).setName( value );
    }
   
    // common routines for name mangling and unmangling

    public String mangleName( String name ) {
        return Util2.mangleId( name, _source );
    }

    String originalName( String arg ) {
        int pos = arg.indexOf( "$$" );
        if ( pos >= 0 )
            return arg.substring( 0,pos );
        else
            return arg;
    }

    public void compose( AstNode etree,  JTSParseTree base,
                 JTSParseTree ext ) {
        if ( arg == null )
            return;
        for ( int i=0; i<arg.length; i++ )
            if ( arg[i]!=null )
                arg[i].compose( etree.arg[i], base, ext );
    }

    public void prepare( JTSParseTree t ) {
        if ( arg == null )
            return;
        for ( int i=0; i<arg.length; i++ )
            if ( arg[i]!=null )
                arg[i].prepare( t );
    }

}
