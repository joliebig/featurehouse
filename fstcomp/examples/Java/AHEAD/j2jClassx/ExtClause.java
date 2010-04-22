

import java.util.*;
import java.io.*;

public class ExtClause {
    public String GetName() {
        return ( ( AST_QualifiedName ) arg[0] ).GetName();
    }
}
