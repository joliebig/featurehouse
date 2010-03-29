abstract public class node {
    /**
     * left or first argument of a boolean operator
     */
    public   node left = null;

    /**
     * right or second argument of a boolean operator
     */
    public    node right = null;

    /**
     * expands all non-primitive operators (implies, etc.)
     * and returns an equivalent boolean expression
     */
    public   abstract node simplify();
    /**
     * returns string of expression 
     */
    public abstract String toString();
    /**
     * returns conjunctive-normal-form string of cnf expression
     */
    public    abstract String cnf2String();
    /**
     * returns deep copy 
     */
    public    abstract node klone();
    /**
     * converts simplified expression into conjunctive-normal-form
     * t = new ...;
     * t = t.simplify();
     * t = t.cnf();
     */
    public    abstract node cnf();

    public String array2String( Object[] o, String op ) {
        String result = "("+ ((node) o[0]).toString();
        for (int i=1; i<o.length; i++ )
           result = result + op + ((node)o[i]).toString();
        return result+")"; 
   }
}
