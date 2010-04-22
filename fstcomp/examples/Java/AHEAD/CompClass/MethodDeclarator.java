

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class MethodDeclarator {

    public String getName() {
        return arg[0].tok[0].tokenName();
    }

    public String signature() {
        String methName = getName();
        AST_ParList p = ( AST_ParList ) arg[1].arg[0];
        String sig="";
        if ( p!=null )
            sig = p.signature();
        return methName + "(" + sig;
    }

    public String selfDeclarator( String unmangled ) {
        return " " + unmangled + "( " + 
                    arg[1].toString() + " )" + arg[2].toString();
    }

    public String selfCall( String mangled ) {
        String params = "";

        // if there is a parameter list, generate it
      
        if ( arg[1].arg[0] != null )
            params = ( ( AST_ParList ) arg[1].arg[0] ).onlyParams();

        return mangled + "( " + params + " )";
    }
}
