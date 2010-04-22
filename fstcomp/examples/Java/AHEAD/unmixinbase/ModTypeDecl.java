

import java.util.*;
import Jakarta.util.*;
import java.io.*;

public class ModTypeDecl {

    // note: the only way that extractable, referenced below,
    //       will be true, is if this statement was preceded
    //       by a SoUrCe statement.

    public void propagateChanges() {
        if ( extractable )
            ( ( UnmodifiedTypeDeclaration ) arg[1] ).propagateChanges();
    }

    public boolean canExtract() {
        extractable = true;
        return true;
    }
}
