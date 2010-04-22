

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class DecNameDim {

    public String Signature() {
	    Dims d = (Dims) arg[1].arg[0];
	    if (d != null) 
		    return d.Signature();
		 else
		    return "";
	 }
}
