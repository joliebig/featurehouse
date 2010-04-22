

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class FormParDecl {
    public String GetSignature() {
        String typeName = ( ( AST_TypeName ) arg[0] ).GetSignature();
        String dims = ( ( VariableDeclaratorId ) arg[1] ).GetDims();
        return typeName + dims;
    }
}
