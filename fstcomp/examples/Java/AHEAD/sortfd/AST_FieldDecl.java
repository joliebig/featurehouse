

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// This layer adds the capability to sort the AST_FieldDecl
// members of a class declaration on the basis of a sort key.
// this is useful when generating lots of related methods --
// one can group them so that debugging and understanding
// the generated code is "easier" for programmers.

// the design is simple: AST_FieldDecl is a list of ClassBodyDeclarations
// a sort key is assigned to ClassBodyDeclarations

public class AST_FieldDecl {

    // sorts the list of class body declarations
    // on their sort key

    public  AST_FieldDecl BodySort() {
        LinkedList l = new LinkedList();
        AstCursor k = new  AstCursor();

        // Step 1: place each classBodyDecl onto a list
        //         and assign sort key to each classBodyDecl

        for ( k.FirstElement( this ); k.MoreElement(); k.NextElement() ) {
            ClassBodyDeclaration cbd = 
               ( ClassBodyDeclaration ) k.node;
            cbd.setSortKey();
            l.add( cbd );
        }

        // Step 2: sort the list -- for a comparator, instantiate
        //         any concrete subclass of ClassBodyDeclaration.

        Collections.sort( l, new FldVarDec() );

        // Step 3: now create a new AST_FieldDecl (list) with the
        //         sorted classBodyDeclarations

        AST_FieldDecl fd = new  AST_FieldDecl();
        ListIterator li = l.listIterator();
        while ( li.hasNext() ) {
            ClassBodyDeclaration eee = ( ClassBodyDeclaration ) li.next();
            eee.Detach();
            fd.add( new  AST_FieldDeclElem().setParms( eee ) );
        }

        // Step 4: finally, replace the original AST_FieldDecl with
        //         the sorted one.  

        return ( AST_FieldDecl ) Replace( fd );
    }
}
