

import java.util.*;
import java.io.*;

//------------------------ j2jIntx layer -------------------
//       encapsulates refinement of interfaces and anything
//       to do with their composition.  in this case, the j2j tool
//       requires some rewrites of interfaces *prior* to their
//       reduction.  Also, the j2j tool will be able to parse extensions
//       to interfaces , but will flag them as errors.

public class UmInterDecl {

    public void harvestConstructors() {
    // do nothing-- there are no constructors in interfaces!
    }

// use inherited reduce2java method - don't even define
// such a method!
}
