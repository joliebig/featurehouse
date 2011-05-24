

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class Env {
    public void execute( int stage ) {
        if ( stage != 0 ) {
            super.executeBypass( stage );
            return;
        }
    }
}
