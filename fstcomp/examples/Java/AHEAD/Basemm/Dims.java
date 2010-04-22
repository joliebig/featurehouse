

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class Dims {
    public String GetSignature() {
        String result = "";

        AstCursor c = new  AstCursor();
        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() )
            result = result + "[]";
        return result;
    }
}
