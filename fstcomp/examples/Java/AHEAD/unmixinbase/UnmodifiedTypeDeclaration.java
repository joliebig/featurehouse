

import java.util.*;
import Jakarta.util.*;
import java.io.*;

// this is the method that UnmodifiedTypeDeclarations need to implement
// in some sense, this is an abstract method, and other layers 
// provide implementations of this method for the appropriate
// UnmodifiedTypeDeclaration.

public class UnmodifiedTypeDeclaration {
    public void propagateChanges() {
        AstNode.override( "UnmodifiedTypeDeclaration.propageChanges" + 
                         kernelConstants.globals().unmixin.fileName, this );
    }
}
