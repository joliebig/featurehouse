public class iff extends node {
    public     iff( node l, node r ) {
        left = l;
        right = r;
    }

    public     node klone() {
        return new iff( left.klone(),right.klone() );
    }

    public     node simplify() {
        // a iff b => (a->b) and (b->a)
        node t = new and( new implies( left, right ),
                            new implies( right.klone(), left.klone() ) );
        return t.simplify();
    }

    public String toString() {
        return "(" + left + " iff " + right + ")";
    }

    public     String cnf2String() {
        System.out.println( "should not call" );
        System.exit( 1 );
        return null;
    }

    public     node cnf() {
        System.out.println( "should not call" );
        System.exit( 1 );
        return null;
    }
}
