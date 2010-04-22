

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public abstract class AstList {
    public int getFirstLineNum() {
        AstNode l;

        // Step 1: return if the list is empty

        if ( arg[0] == null )
            return -1;

        // Step 2: traverse the list

        for ( l = arg[0]; l != null; l = l.right ) {
            if ( l.arg[0] == null )
                continue;

            int result = l.arg[0].getFirstLineNum();
            if ( result != -1 )
                return result;
        }
        return -1;
    }

    public int getLastLineNum() {
        AstNode l;

        // Step 1: return if the list is empty

        if ( last == null )
            return -1;

        // Step 2: traverse the list backwards

        for ( l = last; l != null; l = l.left ) {
            if ( l.arg[0] == null )
                continue;

            int result = l.arg[0].getLastLineNum();
            if ( result != -1 )
                return result;
        }
        return -1;
    }
}
