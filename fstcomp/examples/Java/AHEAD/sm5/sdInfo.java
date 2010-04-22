

import java.util.Hashtable;
import java.util.Vector;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// instantiated once per state machine declaration

class sdInfo implements Serializable {
    public String              name; // name of state machine
    public String              superSm_name; // name of smachine's parent
    public SmContainer         TransCont; // container of transitions
    public SmContainer         StateCont; // container of states
    public  AstNode       extends_ast; //
    public  AstNode       impls_ast; //
    public  AST_FieldDecl body_ast; //
    public  AST_ParList   pardecl_ast; // AST of delivery's par list
    public String              pardecl; // Str of delivery's par list
    public  AST_ArgList   argdecl_ast; // AST of delivery's arg list
    public String              argdecl; // Str of delivery's arg list
    public String              state_constants; // constant decls
    public  AST_Stmt      otherwise_default_ast; //
    public  AST_Stmt      no_trans_ast; //
    public String              extra_methods; // added by refinements
    public  sdInfo        next; // sdInfo of superSm_name

    public sdInfo() {
        name           = null;
        superSm_name   = null;
        TransCont       = new SmContainer( (sdInfo) this, false );
        StateCont      = new SmContainer( (sdInfo) this, true );
        extends_ast    = null;
        impls_ast      = null;
        body_ast       = null;
        pardecl_ast    = null;
        pardecl        = "";
        argdecl_ast    = null;
        argdecl        = "";
        state_constants= "";
        otherwise_default_ast = null; // set in UmodSmDecl
        no_trans_ast   =(AST_Stmt) AstNode.markStack(AstNode.aliasStack.size(),  (AST_Stmt) new AST_Stmt()

.add( (AST_StmtElem) new AST_StmtElem().setParms( (ExprStmt) new ExprStmt().setParms( 
 (PrimExpr) new PrimExpr().setParms( 
 (PPQualName) new PPQualName().setParms( 
 (AST_QualifiedName) new AST_QualifiedName()

.add( (AST_QualifiedNameElem) new AST_QualifiedNameElem().setParms( (NameId) new NameId().setParms(new AstToken().setParms("\r\n        ","System", 0)) /* NameId */
))/* AST_QualifiedNameElem + add */

.add( (AST_QualifiedNameElem) new AST_QualifiedNameElem().setParms(new AstToken().setParms("",".", 0),
 (NameId) new NameId().setParms(new AstToken().setParms("","err", 0)) /* NameId */
))/* AST_QualifiedNameElem + add */

.add( (AST_QualifiedNameElem) new AST_QualifiedNameElem().setParms(new AstToken().setParms("",".", 0),
 (NameId) new NameId().setParms(new AstToken().setParms("","println", 0)) /* NameId */
))/* AST_QualifiedNameElem + add */
) /* PPQualName */
,  (Suffixes) new Suffixes()

.add( (SuffixesElem) new SuffixesElem().setParms( (MthCall) new MthCall().setParms( 
 (Args) new Args().setParms( 
new AstToken().setParms("","(", 0),  new AstOptNode(
).setParms( (AST_ArgList) new AST_ArgList()

.add( (AST_ArgListElem) new AST_ArgListElem().setParms( (AddExpr) new AddExpr().setParms( 
 (StrLit) new StrLit().setParms( 
new AstToken().setParms(" ","\"Unrecognizable_state\"", 0)) /* StrLit */
,  (MoreAddExpr) new MoreAddExpr()

.add( (MoreAddExprElem) new MoreAddExprElem().setParms( (AdEBod) new AdEBod().setParms( 
 (Plus) new Plus().setParms( 
new AstToken().setParms(" ","+", 0)) /* Plus */
,  (PPQualName) new PPQualName().setParms( 
 (AST_QualifiedName) new AST_QualifiedName()

.add( (AST_QualifiedNameElem) new AST_QualifiedNameElem().setParms( (NameId) new NameId().setParms(new AstToken().setParms(" ","current_state", 0)) /* NameId */
))/* AST_QualifiedNameElem + add */
) /* PPQualName */
) /* AdEBod */
))/* MoreAddExprElem + add */
) /* AddExpr */
))/* AST_ArgListElem + add */
) /* AstOptNode */
, new AstToken().setParms(" ",")", 0)) /* Args */
) /* MthCall */
))/* SuffixesElem + add */
) /* PrimExpr */
, new AstToken().setParms("",";", 0)) /* ExprStmt */
))/* AST_StmtElem + add */

.add( (AST_StmtElem) new AST_StmtElem().setParms( (ExprStmt) new ExprStmt().setParms( 
 (PrimExpr) new PrimExpr().setParms( 
 (PPQualName) new PPQualName().setParms( 
 (AST_QualifiedName) new AST_QualifiedName()

.add( (AST_QualifiedNameElem) new AST_QualifiedNameElem().setParms( (NameId) new NameId().setParms(new AstToken().setParms("\r\n        ","System", 0)) /* NameId */
))/* AST_QualifiedNameElem + add */

.add( (AST_QualifiedNameElem) new AST_QualifiedNameElem().setParms(new AstToken().setParms("",".", 0),
 (NameId) new NameId().setParms(new AstToken().setParms("","exit", 0)) /* NameId */
))/* AST_QualifiedNameElem + add */
) /* PPQualName */
,  (Suffixes) new Suffixes()

.add( (SuffixesElem) new SuffixesElem().setParms( (MthCall) new MthCall().setParms( 
 (Args) new Args().setParms( 
new AstToken().setParms("","(", 0),  new AstOptNode(
).setParms( (AST_ArgList) new AST_ArgList()

.add( (AST_ArgListElem) new AST_ArgListElem().setParms( (IntLit) new IntLit().setParms( 
new AstToken().setParms(" ","1", 0)) /* IntLit */
))/* AST_ArgListElem + add */
) /* AstOptNode */
, new AstToken().setParms(" ",")", 0)) /* Args */
) /* MthCall */
))/* SuffixesElem + add */
) /* PrimExpr */
, new AstToken().setParms("",";", 0)) /* ExprStmt */
))/* AST_StmtElem + add */
).patch()
;
        extra_methods  = "";
        next           = null;
    }

    // used for serialization - throw away anything that is not needed.
    // this object is serialized, and is read in during the processing
    // of parent state machines in inheritance hierarchies.  Not all
    // information is needed -- most is thrown away.
    // REMEMBER: if an AST is to be saved/serialized, it must be
    // Detached from the tree

    public void truncate() {
        // leave name
        // leave superSm_name
        // truncate ALL transitions, because we may have refined some
        // previously defined ones, some of which are in memory...
        TransCont.truncate();
        StateCont.truncate();
        extends_ast = null;
        impls_ast = null;
        body_ast = null;
        pardecl_ast.Detach();
        // pardecl leave as is
        argdecl_ast.Detach();
        state_constants = "";
        otherwise_default_ast = null;
        no_trans_ast = null;
        extra_methods = null;
        next = null;
    }

    // only for debugging

    public void print() {
        int i;
        sdInfo s;

        System.out.println( "Sm Name " + name + " refines " + superSm_name );
        System.out.println( "Transitions Container" );
        TransCont.print();
        StateCont.print();
        System.out.println( "extends : " + extends_ast );
        System.out.println( "impls: " + impls_ast );
        System.out.println( "body: " + body_ast );
        System.out.println( "params: "+ pardecl );
        System.out.println( "args: "+ argdecl );
        System.out.println( "state constants " + state_constants );
        System.out.println( "Otherwise Default " + otherwise_default_ast );
        System.out.println( "no trans ast" + no_trans_ast );
        System.out.println( "Extra methods :" + extra_methods );
        System.out.println();
        s = getSuper();
        if ( s!=null )
            s.print();
    }

    // get "superclass" of current sdInfo instance - return null if at root

    public sdInfo getSuper() {
        if ( next != null ) // non-null next pointer is always correct
            return next;

        if ( superSm_name == null )
            return null; // at root

        next = ( sdInfo ) Utility.readObjectFromFile( Utility.SerFileName( superSm_name ) );
        return next;
    }
}
