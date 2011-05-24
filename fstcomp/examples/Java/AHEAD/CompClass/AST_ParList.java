

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

//-------------- for creating calls to self -------------------

public class AST_ParList {

    // this only works for a particular combination of productions.
    // I'm lazy not pushing all of this out into its full generality

    public String onlyParams() {
        AstCursor c = new  AstCursor();
        String result = "";

        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            // here's what I'm taking advantage of -- I know
            // a formal parameter will always be a FormParDecl production
            // and that a VariableDeclaratorId will always be a
            // DecNameDim production.  To generalize, it is necessary
            // to push a set of abstract methods through a set of classes...

            FormParDecl fpd = null;
            DecNameDim  dnd = null;
            fpd = ( FormParDecl ) c.node;
            dnd = ( DecNameDim ) fpd.arg[1];

            String varname = dnd.arg[0].tok[0].tokenName();
            if ( result.equals( "" ) )
                result = varname;
            else
                result = result + ", " + varname;
        }
        return result;
    }
}
