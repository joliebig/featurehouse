

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

//---------------------- AST_FIELD declarations-------------------

public class AST_FieldDecl {

    // used for typeSorting and and in baseRewrites

    public  AstList makeList( AstNode n ) {
        if ( n instanceof  MethodDeclaration ) {
            n = ( AstNode ) n.clone();
        }

        AST_FieldDecl l = new  AST_FieldDecl();
        l.add( new  AST_FieldDeclElem().setParms( n ) );
        return l;
    }

    public void compose( AstNode etree ) {
        AstNode.fatalError( "don't invoke AST_FieldDecl.compose; use static" + 
                    " AST_FieldDecl.compose( AstNode etree, LinkedList ll )" );
    }

    // the following compose method is applied to a base node of
    // type AST_FieldDecl.  It has two parameters: the corresponding
    // AST_FieldDecl of the base AND a LinkedList of extension code
    // fragments that may have Base() references.  The trees in this 
    // vector are harvested from declarative specs (such as 
    // edge declarations in State Machine).  These trees are not
    // part of the extension code proper, but do belong there and
    // should be transformed just as if they were part of the 
    // extension AST_FieldDecl

    // part of the complexity of this method is that we blindly merged
    // the classbodies of the base and extension.  If the base was empty,
    // we substituted the ClassBody of the extensioin.  We can't quite
    // do this because we should check for errors in the extension 
    // itself.

    // FINALLY -- when you call this method, remember that it returns
    // the updated node.  so a call ALWAYS looks like:
    //    arg[?] = $TEqn.AST_FieldDecl( arg[?], ... , ... );
    // otherwise results aren't propagated!

    public static  AstNode compose( AstNode btree, 
                                           AstNode etree, LinkedList ll ) {
        ListIterator li = null;

        // Step 0: validate correct type

        AST_FieldDecl b = ( AST_FieldDecl ) btree;
        AST_FieldDecl e = ( AST_FieldDecl ) etree;
        if ( ll != null )
            li = ll.listIterator();

        // Step 1: collect signatures of all class member declarations
        //         in both the base and extension declarations

        Hashtable          hb = new Hashtable();
        Hashtable          he = new Hashtable();
        AstCursor     c = new  AstCursor();

        if ( b != null ) {
            for ( c.FirstElement( b ); c.MoreElement(); c.NextElement() ) {
                ( ( ClassBodyDeclaration ) c.node ).add2Hash( hb );
            }
        }

        if ( e != null ) {
            for ( c.FirstElement( e ); c.MoreElement(); c.NextElement() ) {
                ( ( ClassBodyDeclaration ) c.node ).add2Hash( he );
            }
        }

        // Step 2: invoke baseRewrite to rewrite all "Base(...).method(...)"
        //         invocations in the extension - both the extension proper
        //         and on the list itself

        if ( e != null )
            e.baseRewrite( hb, he, 0 );
        if ( li != null ) {
            while ( li.hasNext() ) {
                AstNode n = ( AstNode ) li.next();
                if ( n != null ) {
                    n.baseRewrite( hb, he, 0 );
                }
            }
        }

        // Step 3: now see if it is Ok to merge each element of the
        //         extension with the base

        Enumeration en = he.elements();
        for ( ; en.hasMoreElements(); ) {
            AstNode elem = ( AstNode ) en.nextElement();
            elem.ok2compose( 0, hb );
        }
         
        // Step 4: perform any cleanup activity on the base declarations

        if ( b != null )
            for ( c.FirstElement( b ); c.MoreElement(); c.NextElement() )
                ( ( ClassBodyDeclaration ) c.node ).cleanUpBase( c, he );
     
        // Step 5: return if both b and e are null, otherwise
        //         merge the extension and base Classbodies

        if ( b == null && e == null )
            return null;
        else
            if ( b != null && e != null )
                b.add( e );
            else
                if ( b == null )
                    b = e;
        // if e == null, use the current value of b which we know is nonnull

        // Step 6: optionally sort the declarations

        if ( Main.keySort ) {
            b = b.BodySort();
        }
        else
            if ( Main.typeSort )
                b.typeSort( new  AST_FieldDecl() );

        return b;
    }
}
