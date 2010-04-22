

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class AstOptNode {
    public void harvestConstructors( int stage ) {
        if ( arg[0]!=null )
            arg[0].harvestConstructors( stage );
    }
}
