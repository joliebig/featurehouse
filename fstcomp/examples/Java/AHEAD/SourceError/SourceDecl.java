

import java.util.*;
import java.io.*;

public class SourceDecl {
    public void checkForErrors( String file ) {
        AstNode.error( tok[0],
                 "JamPack cannot compose Mixin-produced files." +
                 " Such files have SoUrCe statements" );
    }
}
