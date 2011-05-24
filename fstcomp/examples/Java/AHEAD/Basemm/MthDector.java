

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class MthDector {
    public String GetSignature() {
        String name = ( ( QName ) arg[0] ).GetName() + "(";
        if ( arg[1].arg[0] != null )
            name = name + ( ( AST_ParList ) arg[1].arg[0] ).GetSignature();
        name = name + ")";
        return name;
    }

	 public String GetDims() {
	     Dims d = (Dims) arg[2].arg[0];
		  return (d==null)? "" : d.GetSignature();
    }
}
