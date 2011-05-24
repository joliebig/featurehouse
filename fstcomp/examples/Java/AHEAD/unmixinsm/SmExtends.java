

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class SmExtends {
    public  SmExtendsClause repairExtendsClause( String myName ) {
        String xname = ( ( AST_QualifiedName ) arg[0].arg[0] ).GetName();
        if ( xname.equals( myName ) )
            return null;
        else
            return ( SmExtendsClause ) this ;
    }
}
