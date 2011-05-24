

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class TransitionDecl {

    // add a transition to the collective

    public static  MMOutput addTrans( String name, boolean inFile, 
                 AstNode me ) {
        String defines = inFile ?  MMGlobals.Defines :  MMGlobals.Refines;
        String noInfo =  MMGlobals.NoInfo;
        String method =   MMGlobals.Method;
        MMOutput m =  Main.mmresult;
 
        MMOutput o = new  MMOutput().init( name,
                                 MMGlobals.Transit,
                                defines );
        o.setlines( me.getFirstLineNum(), me.getLastLineNum() );

        o.nested.putUnique( "Action", 
                  new  MMOutput().init( "Action", method, noInfo ) );
        o.nested.putUnique( "Condition", 
                  new  MMOutput().init( "Condition", method, noInfo ) );
        m.nested.putUnique( name + ":", o );
        return o;
    }

    public void execute( int stage ) {
        if ( stage!=0 ) {
            super.execute( stage );
            return;
        }

        NamedVector n;

        // Step 1: harvest information

        String name = ( ( QName ) arg[0] ).GetName();
        String start = ( ( StartName ) arg[1] ).GetName();
        String end = ( ( QName ) arg[2] ).GetName();

        // Step 2: define MMOutput for it and add it to main.

        MMOutput o = addTrans( name, true, this );

        // Step 3: now define the signatures

        n = new  NamedVector( MMGlobals.StartState );
        n.add( start );
        o.union( n );
        n = new  NamedVector( MMGlobals.EndState );
        n.add( end );
        o.union( n );
    }
}
