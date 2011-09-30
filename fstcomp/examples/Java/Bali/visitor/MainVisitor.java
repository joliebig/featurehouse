

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MainVisitor extends AstVisitor {

    public void visit( BaliGrammarNode node ) {
        rules.add( node.tok[0].getTokenName() ) ;
    }

    final public List rules = new ArrayList() ;
}
