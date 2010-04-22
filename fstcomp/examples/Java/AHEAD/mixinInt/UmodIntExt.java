

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class UmodIntExt {

    // return interface declaration with inheritance

    public  UnmodifiedTypeDeclaration prepareReplace( JTSParseTree t ) {

        // Step 1: create replacement interface declaration

        UmInterDecl uid = new  UmInterDecl()
                .setParms( ( AstToken ) tok[0], ( QName ) arg[0], 
                ( AstOptNode ) arg[1], ( AstToken ) tok[1], 
                ( AstOptNode ) arg[2], ( AstToken ) tok[2] );

        // Step 2: set the source of this new node to be equal to 
        //         that of the original

        uid._source = getSource();

        // Step 3: also, take the comment of the original node
        //         and preface it onto this new node

        uid.prependComment( up.getComment() );

        // Step 4: return final result

        return uid;
    }

}
