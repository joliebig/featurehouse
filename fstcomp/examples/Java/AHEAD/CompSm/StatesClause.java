

import java.util.*;
import java.io.*;

public class StatesClause {

    public void add2Hash( Hashtable h, String source ) {
        AstNode.override( "add2Hash", this );
    }

    public void defineState( Hashtable h, String name, String source ) {
        String statekey = "state " + name;
        String result = ( String ) h.get( statekey );
        if ( result==null )
            h.put( statekey,source );
        else
            AstNode.error( tok[0], statekey + 
                     " has multiple definitions -- see file " + source );
    }
}
