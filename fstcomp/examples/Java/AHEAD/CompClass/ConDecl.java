

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class ConDecl {

    public void ok2compose( int stage, Hashtable hb ) {}

    public String signature() {
        // Signature of a constructor is "<name>(<signature of arguments>"

        // Step 1: get name

        String result = arg[1].tok[0].tokenName() + "(";

        // Step 2: add signature of arguments 
        AST_ParList p = ( AST_ParList ) arg[2].arg[0];
        if ( p != null )
            result = result + p.signature();

        return result;
    }

    public void add2Hash( Hashtable h ) {
        h.put( signature(), this );
    }

    // cleanUpBase is where composition of constructors and their refinements
    // is performed.

    public void cleanUpBase( AstCursor k, Hashtable he ) {

        // Step 1: see if extension has a constructor that can be composed.

        String sig = signature();
        AstNode c = ( AstNode ) he.get( sig );

        if ( c == null )
            return;

        if ( c instanceof  RefCons ) {

            // Step 2: get variables for the base code and extension code
            //         both are of type AST_Stmt

            AstList baseCode = ( AstList ) arg[5].arg[0];
            AstList extCode  = ( AstList ) c.arg[2].arg[0];

            // Step 3: create the new body of the constructor

            AST_Stmt body =(AST_Stmt) AstNode.markStack(AstNode.aliasStack.size(),  (AST_Stmt) new AST_Stmt()

.add( (AST_StmtElem) new AST_StmtElem().setParms( (BlockC) new BlockC().setParms( 
new AstToken().setParms(" ","{", 0),  new AstOptNode(
).setParms( (AST_Stmt) new AST_Stmt()

.add( (AST_Stmt) AstNode.addComment( AstNode.safeCopy( baseCode),"\r\n                ")) 
) /* AstOptNode */
, new AstToken().setParms("\r\n            ","}", 0)) /* BlockC */
))/* AST_StmtElem + add */

.add( (AST_StmtElem) new AST_StmtElem().setParms( (BlockC) new BlockC().setParms( 
new AstToken().setParms(" ","{", 0),  new AstOptNode(
).setParms( (AST_Stmt) new AST_Stmt()

.add( (AST_Stmt) AstNode.addComment( AstNode.safeCopy( extCode),"\r\n                ")) 
) /* AstOptNode */
, new AstToken().setParms("\r\n            ","}", 0)) /* BlockC */
))/* AST_StmtElem + add */
).patch()
;

            // Step 4: replace old body with new body

            arg[5].Replace( body );
                 
            // Step 5: delete refinement from extension

            c.Delete();
        }
        else
            if ( c instanceof  ConDecl )
                AstNode.error( arg[1].tok[0],
                           "Overriding of constructor with signature " + sig +
                                ") not permitted" );
            else
                AstNode.error( arg[1].tok[0],
                               "Unrecognized constructor declaration with signature " +
                               sig + ") in extension" );
    }

    public void compose( AstNode etree ) {
        AstNode.fatalError( arg[1].tok[0],
                "cannot redefine constructor " + signature() + ")" );
    }
}
