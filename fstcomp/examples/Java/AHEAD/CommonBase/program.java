

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util;
import java.io.*;

public class program       {

    // return layer name

    public String getAspectName() {
        if ( arg[0].arg[0] == null ) {
            String name =  Main.deriveLayerName();
            return name;
        }
        // assume arg[0].arg[0] is of type PackStm
        return ( ( AST_QualifiedName ) arg[0].arg[0].arg[0] ).GetName();
    }

    // set layer name

    public void setAspectName( String pname ) {
	// if there is no package declaration, then create one
	// else change the first entry of the AST_QualifiedName
	//

        if (arg[0].arg[0] == null) {
            AST_Program p = AST_Program.MakeAST("layer " + pname +";\n");
            arg[0].Replace( p.arg[0] );
        }
        else {	
           // arg[0] is optional node
           // arg[0].arg[0] is PackageDeclaration
           // arg[0].arg[0].arg[0] is AST_QualifiedName

           ((AST_QualifiedName) arg[0].arg[0].arg[0]).setPrefixName( pname );
	   return;
        }
    }
/*
        String prog = "layer " + pname + ";\n";
        AST_Program p =  AST_Program.MakeAST( prog );
        if ( arg[0].arg[0] != null )
            p.addComment( this.getComment() );
        arg[0].Replace( p.arg[0] );
    }
*/
}
