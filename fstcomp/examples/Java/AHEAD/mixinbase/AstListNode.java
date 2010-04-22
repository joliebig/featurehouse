

import java.util.*;
import Jakarta.util.*;
import java.io.*;

public abstract class AstListNode {

    public void setSource( String s ) {
        AstNode.fatalError( "AstListNode.setSource( s ) method should not be called" );
    }

    public void compose( AstNode etree,  JTSParseTree base,
             JTSParseTree ext ) {
        AstNode.fatalError( "AstListNode.compose method should not be called" );
    }

    public void prepare( JTSParseTree t ) {
        AstNode.fatalError( "AstListNode.prepare method should not be called" );
    }
}
