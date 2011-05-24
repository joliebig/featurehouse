public class not extends node {
    public     not( node t ) {
        left = t;
    }

    public     node klone() {
        return new not( left.klone() );
    }

    public     node simplify() {
        node temp = null;

        if ( left instanceof bterm )
            return this;

        // not not a => a
        if ( left instanceof not )
            return left.left.simplify();

        // not ( a and b ) => not a or not b
        if ( left instanceof and ) {
            temp = new or( new not( left.left ), new not( left.right ) );
            return temp.simplify();
        }

        // not ( a or b ) => not a and not b
        if ( left instanceof or ) {
            temp = new and( new not( left.left ), new not( left.right ) );
            return temp.simplify();
        }

        // not (something) -- just simplify something and try again.
        // eventually the expression will reduce to one of the above
        // cases
        left = left.simplify();
        return simplify();
    }

    public String toString() {
        if ( left instanceof bterm )
            return "not " + left;
        else
            return "not (" + left + ")";
    }

    public     String cnf2String() {
        return "not " + left.cnf2String();
    }

    public     node cnf() {
        // rule -- not should apply only to individual terms by now
        // that should be the result of repeated simplifications
        //
        if ( left instanceof bterm )
            return this;
        System.err.println( "not simplified" );
        System.exit( 1 );
        return null;
    }
}
