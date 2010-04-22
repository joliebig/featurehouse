layer collect;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Parser code blocks are implemented as a {@link List} of {@link String}
 * objects representing the collected code.  They are stored in order of
 * declaration.
 *
 * @layer<collect>
 */
    
public class ParserBlocksData extends ArrayList {

    public void addNode( ParserCodeNode node ) {
        String block = node.arg[0].arg[0].tok[0].getTokenName() ;
        add( block ) ;
    }

}
