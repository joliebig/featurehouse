

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class AstOptNode {
    public void baseRewrite( Hashtable hb, Hashtable he, int stage ) {
        if ( arg[0]!=null )
            arg[0].baseRewrite( hb, he, stage );
    }
}
