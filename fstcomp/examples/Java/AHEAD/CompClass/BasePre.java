

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class BasePre {

    boolean isMangled( String arg ) {
        return Util2.isMangled( arg );
    }

    String mangleName( MethodDeclaration m ) {
        return Util2.mangleId( m.GetName(), m.getSource() );
    }

    String originalName( String arg ) {
        return Util2.unmangleId( arg );
    }

    public void baseRewrite( Hashtable hb, Hashtable he, int stage ) {

        // baseRewrite does the following:
        // (a) creates the String signature of the base method
        //     being called.
        // (b) mangles the name of the base method although leaves
        //     its signature in the hash table intact.  Also, adds
        //     the "final" modifier to the base method (as it will
        //     never be extended again).
        // (c) replaces "Base(t1,t2...).methcall(...)"
        //     with "methcall$$xxx(...)", where "methcall$$xxx" is
        //     the mangled name of the base method
        // (d) recurse on revised parse tree

        // Step 1: do nothing if stage !== 0
        //         this arises if we are in the middle of an AST 
        //         constructor

        if ( stage != 0 ) {
            super.baseRewrite( hb,he,stage );
            return;
        }
        
        // Step 2: (a) get signature of base method being called
        //         Base(a,b,c).m(c,d) - signature is "m(a,b,c"

        String typeSig = "";
        if ( arg[0].arg[0] != null )
            typeSig = ( ( AST_TypeNameList ) arg[0].arg[0] ).signature();
        String mthsig = arg[1].tok[0].tokenName() + "(" + typeSig;

        // Step 3: (b) find base method that is being called.  If there is
        //         no such method, issue a warning, and replace 
        //         Base(...).method with super.method

        MethodDeclaration m = ( MethodDeclaration ) hb.get( mthsig );
        if ( m==null ) {

            AstNode.warning( tok[0], 
                      " refinement makes call to non-existent base method "
                  + mthsig + ")" );

            SuperPre s = new  SuperPre()
              .setParms( new  AstToken().setParms( getComment(),"super", 0 ), 
              new  AstToken().setParms( "",".", 0 ),  
              ( QName ) arg[1] );
            this.up.arg[0].Replace( s );
            return;
        }

        // Step 4: remember original name and mangled name of base method
        //         also, see if the base method is overridden.

        MethodDeclaration me = ( MethodDeclaration ) he.get( mthsig );
        m.mangledName = mangleName( m );
        m.overriddenBy = me;
        m.isOverridden = ( me!=null );
        m.isReferenced = true;

        // Step 5: now replace "Base(...).meth(...)" with
        //         meth$$() -- yes, this is ugly, but I don't know
        //         any better way of doing it.  I found out what to
        //         use by running jak on exp{ mthcall(...) }exp
        //         and exp{ super.mthcall(...) }exp and seeing what
        //         was generated

        String cmt = getComment();
        PPQualName p = new  PPQualName().setParms( ( AST_QualifiedName )
              new  AST_QualifiedName().add( new  AST_QualifiedNameElem().setParms( new  AstToken().setParms( "","", 0 ),
                      new  NameId().setParms( new  AstToken().setParms( " ",m.mangledName, 0 ) ) ) ) ) /* $TEqn.PPQualName */;
        this.up.arg[0].Replace( p );
        p.addComment( cmt, true );
    }
}
