

import java.util.*;
import Jakarta.util.*;
import java.io.*;

// note: default action for lists is that lists of base and extension
//       MUST have the same number of elements!!

public abstract class AstList {

    public void compose( AstNode etree,  JTSParseTree base,
               JTSParseTree ext ) {
        AstNode.override( "AstList.compose", this );
    }

    public void prepare( JTSParseTree t ) {
        AstNode.override( "AstList.prepare", this );
    }

    public void setSource( String s ) {
        AstNode l;
        if( _source == null )
            _source = s;
        if ( arg[0]==null )
            return;
        for ( l = arg[0]; l!=null; l = l.right ) {
            if ( l.arg[0] == null )
                continue;
            l.arg[0].setSource( s );
        }
    }

    public  AstList makeList( AstNode n ) {
        AstNode.override( "AstList.makeList", this );
        return null;
    }
}
