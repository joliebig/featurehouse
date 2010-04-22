

import java.util.*;
import java.io.*;

//------------CompSm Layer ------------

public class ESList {

    public void harvestAst( LinkedList ll ) {
        // foreach element on an ESList, invoke harvestAst

        AstCursor c = new  AstCursor();

        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            ( ( Es ) c.node ).harvestAst( ll );
        }
    }

    public void add2Hash( Hashtable h, String source ) {
        // foreach element on an ESList, enter an element into the
        // hash table and note any replications when replications are
        // errors -- i.e., base defines edge x and refinement defines
        // edge x too is an error.
   
        AstCursor c = new  AstCursor();
 
        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            Es es = ( Es ) c.node;
            es.add2Hash( h, source );
        }
    }

    public void compose( AstNode etree ) {
        // composition is simple -- just concatenate the ESList of the
        // base to the ESList of the extension

        ESList e = ( ESList ) etree;

        add( e );   // formerly addHead -- original definitions come first
    }

}
