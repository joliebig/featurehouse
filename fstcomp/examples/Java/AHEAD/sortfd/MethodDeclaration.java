

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class MethodDeclaration {
    public void setSortKey() {
        setSortKey( GetUnmangledName() );
    }

    public String GetUnmangledName() {
        AstNode.override( "MethodDeclaration.GetUnmangledName", this );
        return null;
    }
}
