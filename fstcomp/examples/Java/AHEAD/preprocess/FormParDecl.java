

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class FormParDecl {
    public String GetName() {
        return ( ( AST_TypeName ) arg[0] ).GetName();
    }
}
