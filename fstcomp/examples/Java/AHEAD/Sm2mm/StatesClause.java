

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class StatesClause {
    public static  MMOutput add( String name, boolean inFile, 
             AstNode me ) {
        // Create an MMOutput for this state and insert it into
        // the main object.  Also, create signatures for each
        // state method and represent this internally as a
        //  nested collective

        MMOutput tmp;

        String defines = inFile ?  MMGlobals.Defines : 
                                    MMGlobals.Refines;
        String noInfo =  MMGlobals.NoInfo;
        String method =   MMGlobals.Method;
        MMOutput m =  Main.mmresult;
 
        MMOutput o = new  MMOutput().init( name,
                                 MMGlobals.State,
                                defines );
        o.setlines( me.getFirstLineNum(), me.getLastLineNum() );
    
        o.nested.putUnique( "Prepare", new  MMOutput().init( "Prepare", method, noInfo ) );
        o.nested.putUnique( "Exit",    new  MMOutput().init( "Exit", method, noInfo ) );
        o.nested.putUnique( "Enter",   new  MMOutput().init( "Enter", method, noInfo ) );
        o.nested.putUnique( "Override", new  MMOutput().init( "Override", method, noInfo ) );
        m.nested.putUnique( name, o );
        return o;
    }
}
