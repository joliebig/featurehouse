

import java.util.*;
import java.io.*;

public class NStateDecl {

    public void add2Hash( Hashtable h, String source ) {
        String n = arg[0].tok[0].tokenName();
        defineState( h, n, source );
    }
}
