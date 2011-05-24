

import java.util.*;
import Jakarta.util.*;
import java.io.*;

public class UnmodifiedTypeExtension {

    // replace extension syntax with corresponding java syntax

    public  UnmodifiedTypeDeclaration prepareReplace( JTSParseTree t ) {
        AstNode.override( "UnmodifiedTypeExtension.prepareReplace", this );
        return null; // pacify whiney compiler
    }
}
