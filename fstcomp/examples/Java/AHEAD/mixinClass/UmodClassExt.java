

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class UmodClassExt {

    // return class declaration that has no extends clause -- yes,
    // even though we know (ultimately) that it will extend another
    // class.  That part is filled in by the extensionOf method above

    public  UnmodifiedTypeDeclaration prepareReplace( JTSParseTree t ) {

        // Step 1: create the replacement class declaration (with
        //         empty extends clause

        UmodClassDecl ucd = new  UmodClassDecl()
                .setParms( ( AstToken ) tok[0], ( QName ) arg[0], 
                          new  AstOptNode(), ( AstOptNode ) arg[1], 
                          ( ClassBody ) arg[2] );

        // Step 2: set the source of this new node to be equal to 
        //         that of the original

        ucd._source = getSource();

        // Step 3: also, take the comment of the original node
        //         and preface it onto this new node

        ucd.prependComment( up.getComment() );

        // Step 4: return final result

        return ucd;
    }
}
