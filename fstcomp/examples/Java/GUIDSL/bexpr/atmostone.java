import java.util.ArrayList;

// predicate that allows at most one to be true

class atmostone extends node {
    public atmostone( node l, node r ) {
        left = l;
        right = r;
    }

    public node klone() {
        return new atmostone( left.klone(),right.klone() );
    }

    public ArrayList children() {
        ArrayList ll;
        ArrayList l = new ArrayList();
        if ( left instanceof atmostone )
            l.addAll( ( ( atmostone ) left ).children() );
        else
            l.add( left );
        if ( right instanceof atmostone )
            l.addAll( ( ( atmostone ) right ).children() );
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
		  if (l == 1)
		     return makeAnd( o, 0, 0 );  // returns true
        for ( int i=0; i<l; i++ )
            for ( int j=i+1; j<l; j++ )
				    if (n==null)
					    n = makeAnd(o,i,j);
					 else
                   n = new and( n, makeAnd( o,i,j ) );
        return n.simplify();
    }

    public String toString() {
        return "atmostone" +array2String( children().toArray(), "," );
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
