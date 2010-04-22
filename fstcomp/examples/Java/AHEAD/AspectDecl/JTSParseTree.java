

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class JTSParseTree {

    // make sure that "layer" token, rather than "package"
    // token is present

    public void preprocessTree( AST_Program root ) throws Exception {
        root.checkAspect( filepath );
        original( root );
    }
}
