public class implies extends node {

    public   node klone() {
        return new implies( left.klone(), right.klone() );
    }

    public    implies( node l, node r ) {
        left = l;
        right = r;
    }

    public    node simplify() {
        // a -> b => not a or b
        node temp = new or( new not( left ), right );
        return temp.simplify();
    }

    public String toString() {
        return "(" + left + ") implies (" + right + ")";
//        return left + " implies " + right;
    }

    public    String cnf2String() {
        // nodes of this type should be simplified by the time
        // cnf2String is called
        System.out.println( "shouldn't be called" );
        System.exit( 1 );
        return null;
    }

    public node cnf() {
        // rule -- implies nodes should be eliminated by simplify
        //
        System.out.println( "SHOULD NEVER BE CALLED -- Expression should be simplified" );
		  try {
		     throw new Exception("Should never be called");
		  }
		  catch (Exception e) {
		     e.printStackTrace();
        }
        System.exit( 1 );
        return null;
    }
}
