import java.util.ArrayList;

// EXPERIMENTAL -- NOT YET USED

// predicate that allows at most one to be true

class choose1 extends node {
    public choose1( node l, node r ) {
        left = l;
        right = r;
    }

    public node klone() {
        return new choose1( left.klone(),right.klone() );
    }

    public ArrayList children() {
        ArrayList ll;
        ArrayList l = new ArrayList();
        if ( left instanceof choose1 )
            l.addAll( ( ( choose1 ) left ).children() );
        else
            l.add( left );
        if ( right instanceof choose1 )
            l.addAll( ( ( choose1 ) right ).children() );
        else
            l.add( right );
        return l;
    }

    private node elem( Object[] o, int i ) {
        return ( ( node ) o[i] ).klone();
    }

    private node makeAnd( Object[] o, int i, int j ) {
        return new not( new and( elem( o,i ), elem( o,j ) ) );
    }
       
    /*
    * x # y # z => (-x^-y^z v -x^y^-z v x^-y^-z )
    */
    public node simplify() {
        node n = null;
        Object[] o = children().toArray();
        int l = o.length;
        if ( l == 1 )
            return (node) o[0];

        node orr = (node) o[0];
        node atmost1 = (node) o[0];

        for ( int i=1; i<l; i++ ) {
           orr = new or( orr, (node) o[i] );
           atmost1 = new atmostone( atmost1, (node) o[i] );
        }
        node result = (new and(orr, atmost1)).simplify();
        return result;
    }

    public String toString() {
        return "choose1" +array2String( children().toArray(), "," );
    }

    public String cnf2String() {
        // return parentheses around non-and arguments
        //
        System.out.println( " should not call " );
        System.exit( 1 );
        return null;
    }

    public node cnf() {
        // rule: anything can sit immediately below ands
        //
        System.out.println( " should not call " );
        System.exit( 1 );
        return null;
    }
}
