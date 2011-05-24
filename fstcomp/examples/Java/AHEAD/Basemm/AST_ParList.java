

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class AST_ParList {
    public String GetSignature() {
        String result = "";
        String comma = "";
        AstCursor c = new  AstCursor();
        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            result = result + comma + ( ( FormalParameter ) c.node ).GetSignature();
            comma = ",";
        }
        return result;
    }
}
