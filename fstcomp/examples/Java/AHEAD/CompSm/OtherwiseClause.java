

import java.util.*;
import java.io.*;

public class OtherwiseClause {
    public void compose( AstNode etree ) {
        AstNode.override( "compose", this );
    }
}
