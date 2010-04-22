

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class NestedInterfaceDeclaration  {
    public void execute( int stage ) {
        if ( stage != 0 ) {
            super.executeBypass( stage );
            return;
        }
		  super.execute(stage);
    }
}
