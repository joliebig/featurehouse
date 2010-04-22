

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class VarDecl  {
    public String GetSignature() {
        return ( ( VariableDeclaratorId ) arg[0] ).GetSignature();
    }
    public String GetDims() {
        return ( ( VariableDeclaratorId ) arg[0] ).GetDims();
    }

	 public String getValue() {
	     AstNode varassign = arg[1].arg[0];
        if (varassign == null) return null;
		  return varassign.toString();
	 }
}
