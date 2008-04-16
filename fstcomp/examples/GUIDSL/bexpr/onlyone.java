import java.util.ArrayList;

class onlyone extends node {
    public    onlyone( node l, node r ) {
        left = l;
        right = r;
    }

    public    node klone() {
        return new onlyone( left.klone(),right.klone() );
    }

    public ArrayList children() {
        ArrayList ll;
        ArrayList l = new ArrayList();
        if ( left instanceof onlyone )
            l.addAll( ( ( onlyone ) left ).children() );
        else
            l.add( left );
        if ( right instanceof onlyone )
            l.addAll( ( ( onlyone ) right ).children() );
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
       
    private node makeOr( Object[] o ) {
        int l = o.length;
        node n = new or( elem( o,0 ), elem( o,1 ) );
        for ( int i=2; i<l; i++ )
            n = new or( n, elem( o,i ) );
        return n;
    }
    /*
    * x # y # z => (-x^-y^z v -x^y^-z v x^-y^-z )
    */
    public node simplify() {
        Object[] o = children().toArray();
        int l = o.length;
        node n = makeOr( o );
        for ( int i=0; i<l; i++ )
            for ( int j=i+1; j<l; j++ )
                n = new and( n, makeAnd( o,i,j ) );
        return n.simplify();
    }
            
    public String toString() {
        return "(" + left + " onlyone " + right + ")";
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
