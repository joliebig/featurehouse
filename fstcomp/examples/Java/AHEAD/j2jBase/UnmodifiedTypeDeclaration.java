

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

// ----------  look elsewhere ---
// there are layers (j2jIntx, j2jSmx, j2jClassx) that define
// the actions specific to j2j tools.  Their contents have been
// removed from j2jbase to support Origami.  The key ideas are that
// you must extend all subclasses of UnmodifiedTypeDeclaration with
// the ability to harvest constructors AND that extensions/refinements
// of these types are parsable, but are illegal.

public class UnmodifiedTypeDeclaration {

    public  conTable inheritedCons;

    public void copyConstructors() {
        // Step 1: copy the inheritedCons of $TEqn.program -- these
        //         are the constructors we want to inherit

        inheritedCons = ( conTable )  kernelConstants.globals().j2jbase.inheritedCons.clone();
        kernelConstants.globals().j2jbase.currentTypeDecl = this;
    }
}
