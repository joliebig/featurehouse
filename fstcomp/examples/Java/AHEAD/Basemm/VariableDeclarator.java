

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

// the following classes are used for accessing variable signatures

public class VariableDeclarator  {
    public String GetSignature() {
        AstNode.override( "VariableDeclarator.GetSignature()", this );
        return null;
    }
    public String GetDims() {
        AstNode.override( "VariableDeclarator.GetDims()", this );
        return null;
	 }

	 public String getValue() {
	     AstNode.override( "VariableDeclarator.getValue()", this );
		  return null;
	 }
}
