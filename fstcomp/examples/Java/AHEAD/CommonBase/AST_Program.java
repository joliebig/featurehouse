

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util;
import java.io.*;

// this layer sits immediately below the JamPack preprocess layer
// and the Mixin mixinbase layer.  it contains the Main and JTSParseTree
// classes that are virtually identical. (So why have multiple copies?)

// the following two class refinements encapsulate the
// methods needed for setting and getting layer names

public class AST_Program {

    // set, get layer name methods

    public String getAspectName() {
        AstNode.override( "AST_Program.getAspectName", this );
        return null;
    }

    public void setAspectName( String pname ) {
        AstNode.override( "AST_Program.setAspectName", this );
    }
}
