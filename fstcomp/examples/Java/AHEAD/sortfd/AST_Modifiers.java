

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class AST_Modifiers {

    public boolean findModifier( String mod ) {
        AstCursor     c = new  AstCursor();
        AST_Modifiers b = ( AST_Modifiers ) this;

        for ( c.FirstElement( b ); c.MoreElement(); c.NextElement() ) {
            if ( ( ( Modifier ) ( c.node ) ).GetName().equals( mod ) )
                return true;
        }
        return false;
    }
}
