

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class program {

    public void checkAspect( String filepath ) {
        AstNode n = arg[0].arg[0];
        if ( n == null )
            return; // OK if it is absent
        if ( n instanceof  AspectStm )
            return;
        else
            AstNode.error( "layer declaration expected; remove package declaration" );
    }
}
