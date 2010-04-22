

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class AstOptNode {
    public int getFirstLineNum() {
        return ( arg[0] == null ) ? -1 : arg[0].getFirstLineNum();
    }

    public int getLastLineNum() {
        return ( arg[0] == null ) ? -1 : arg[0].getLastLineNum();
    }
}
