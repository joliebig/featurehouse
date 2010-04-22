

import java.util.*;
import Jakarta.util.*;
import java.io.*;

public class TypeDeclaration {
    public boolean extractable = false;

    public void propagateChanges() {
        AstNode.override( "TypeDeclaration.propageChanges", this );
    }

    // we expect this method to be overridden in the cases that
    // we can extract classbodies and propagate them to their owners

    public boolean canExtract() {
        extractable = false;
        return false;
    }
}
