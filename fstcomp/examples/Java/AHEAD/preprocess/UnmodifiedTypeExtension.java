

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class UnmodifiedTypeExtension {
    public String GetName() {
        AstNode.override( "UnmodifiedTypeExtension.GetName", this );
        return "";
    }

    public String GetType() {
        AstNode.override( "UnmodifiedTypeExtension.GetType", this );
        return "";
    }
      
    public void compose( AstNode etree ) {
        AstNode.override( "UnmodifiedTypeExtension.compose", this );
    }
}
