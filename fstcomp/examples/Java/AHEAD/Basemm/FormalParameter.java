

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class FormalParameter {
    public String GetSignature() {
        AstNode.override( "MethodDeclarator.GetSignature()", this );
        return null;
    }
}
