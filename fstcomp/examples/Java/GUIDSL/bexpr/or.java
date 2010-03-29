import java.util.ArrayList;

public class or extends node {

    public     or( node l, node r ) {
        left = l;
        right = r;
    }

    public     node klone() {
        return new or( left.klone(), right.klone() );
    }

    public    node simplify() {
        left = left.simplify();
        right = right.simplify();
        return this;
    }

    public String toString() {
        return array2String( children().toArray(), " or " );
    }

    public     String cnf2String() {
        return left.cnf2String() + " or " + right.cnf2String();
    }

    public     node cnf() {
        // if a branch is an or, then we must first recurse on it

        if ( left instanceof or )
            left = left.cnf();
        if ( right instanceof or )
            right = right.cnf();

        // rule in cnf -- ands cannot be below ors
        // the action is then to keep expanding
        //
        // (a ^ b) v c => (a v c) ^ (b v c)
        //
        if ( left instanceof and ) {
            node temp = new and( new or( left.left, right ),
                                                                                                 new or( left.right, right.klone() ) );
            return temp.cnf();
        }
        //
        // c v (a ^ b) => (c v a) ^ (c v b)
        //
        if ( right instanceof and ) {
            node temp = new and( new or( left, right.left ),
                                                                                                 new or( left.klone(), right.right ) );
            return temp.cnf();
        }
        return this;
    }

    public ArrayList children() {
        ArrayList ll;
        ArrayList l = new ArrayList();
        if ( left instanceof or )
            l.addAll( ( ( or ) left ).children() );
        else
            l.add( left );
        if ( right instanceof or)
            l.addAll( ( ( or) right ).children() );
        else
            l.add( right );
        return l;
    }
}
