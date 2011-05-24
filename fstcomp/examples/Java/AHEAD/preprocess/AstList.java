

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// note: default action for lists is that lists of base and extension
//       MUST have the same number of elements!!

public abstract class AstList {
      
    public void compose( AstNode etree ) {
        AstNode.override( "AstList.compose", this );
        AstNode l, ll;
        if ( arg[0]==null )
            return;
        for ( l = arg[0], ll = etree.arg[0]; l!=null; 
             l = l.right, ll = ll.right ) {
            if ( l.arg[0] == null )
                continue;
            l.arg[0].compose( ll.arg[0] );
        }
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

    // typeSort algorithm -- arrange nodes of a list in type
    // order.  Consider the following list, where letters denote
    // types and numbers are instance numbers.
    //     A1 - B1 - B2 - A2 - C2 - B3 - A4 - C3
    // The typeSort of this list is:
    //     A1 - A2 - A4 - B1 - B2 - B3 - C2 - C3
    // That is, all instances of A are listed after the first A (i.e., A1)
    // all instances of B are listed after the first B (i.e., B1)
    // and all instances of C are listed after the first C (i.e., C2)
    // typeSort is introduced to make the result of composing 
    // type declarations easier to read...  This is a linear algorithm

    public void typeSort( AstList listMaker ) {

        AstCursor c = new  AstCursor();
        Hashtable h = new Hashtable();
        AstCursor k;
        String    nodeType;

        // since we got to this point by manipulating lists
        // let's normalize the entire structure

        this.normalize();

        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {

            // Step 1: get name of node type

            nodeType = c.node.getClass().getName();

            // Step 2: now see if we've registered this type before.
            //         if so, all instances of this type line up after
            //         this instance

            k = ( AstCursor ) h.get( nodeType );
            if ( k == null ) {
                // Step 2.1: remember first instance (henceforth "anchor")
                //           by cloning a copy of c and its pointers

                h.put( nodeType, c.copy() );
            }
            else {
                // Step 2.2: remove the current node, but remember it

                AstNode n = c.node;
                c.Delete(); // detach node and have cursor point to next
                             // element on list

                // Step 2.3: create list of length 1 and place the 
                //           previously detached node on this list
        
                AstList l = listMaker.makeList( n );

                // Step 2.4: add this list after the anchor point, and
                //           advance the anchor point 

                k.AddAfter( l );
                k.NextElement();
            }
        }
    }

    public  AstList makeList( AstNode n ) {
        AstNode.override( "AstList.makeList", this );
        return null;
    }
}
