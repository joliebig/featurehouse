

import java.util.*;
import java.io.*;

public class UmodIntExt {

    public void reduce2java( AstProperties props ) {

        // Step 1: get name of parent interface
        String interfaceName = arg[0].tok[0].tokenName();

        // Step 2: and now the reduction
        tok[0].print( props );      // interface
        arg[0].reduce2java(props);  // Qname
        props.print(" extends " + kernelConstants.stub + "." + interfaceName + " ");
		  tok[1].print( props );      // {
        arg[2].reduce2java(props);  // body
		  tok[2].print( props );      // }
    }
}
