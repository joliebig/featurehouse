

import java.util.*;
import java.io.*;

public class SmClassBody {
    public void compose( AstNode etree ) {
        AstNode.override( "compose", this );
    }
}
