

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class CompClassData {

    // isBaseAClass is used for error checking during
    // composition.  If the Base is a class (and not a class
    // extension), value is true.
    public boolean isBaseAClass;

    public int     Initializer_counter = 0;
    public int     ManufacturedName = 0;
}
