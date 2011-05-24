import java.util.ArrayList;

public class and extends node {
    public    and( node l, node r ) {
        left = l;
        right = r;
    }

    public    node klone() {
        return new and( left.klone(),right.klone() );
    }

    public    node simplify() {
        // simplify both sides of an and
        left = left.simplify();
        right = right.simplify();
        return this;
    }

    public String toString() {
        return array2String( children().toArray(), " and " );
    }

    public ArrayList children() {
        ArrayList ll;
        ArrayList l = new ArrayList();
        if ( left instanceof and )
            l.addAll( ( ( and ) left ).children() );
        else
            l.add( left );
        if ( right instanceof and)
            l.addAll( ( ( and ) right ).children() );
        else
            l.add( right );
        return l;
    }
    public    String cnf2String() {
        // return parentheses around non-and arguments
        //
        String result;
        if ( left instanceof and )
            result = left.cnf2String();
        else
            result = "(" + left.cnf2String() + ")";
        if ( right instanceof and )
            result = result + " and " +  right.cnf2String();
        else
            result = result +  " and (" + right.cnf2String() + ")";
        return result;
    }

    public    node cnf() {
        // rule: anything can sit immediately below ands
        //
        left = left.cnf();
        right = right.cnf();
        return this;
    }
}
