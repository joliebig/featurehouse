

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class AST_FieldDecl {

    // this method adds a ConstructorMarker to the end of the
    // AST_FieldDecl.  This marker is used to indicate where the
    // list of inherited constructors are to be placed.
 
    public void addMarker( conTable inheritedCons ) {
 
        // Step 1: create an AST_FieldDecl that contains this lone marker
 
        AST_FieldDecl n = ( AST_FieldDecl ) new  AST_FieldDecl()
            .add( new  AST_FieldDeclElem().setParms( new  ConstructorMarker( inheritedCons ) ) );
 
        // Step 2: now add it to the end of this AST_FieldDecl
 
        add( n );
    }
}
