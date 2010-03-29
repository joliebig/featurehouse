// $Header: /home/apel/cvs/fstcomp/examples/Java/GraphJava/BasicGraph/Graph/Attic/Edge.java,v 1.1 2010-03-29 20:20:28 apel Exp $
/**
 * Please complete these missing tags
 * @author
 * @rref
 * @copyright
 * @concurrency
 * @see
 */
package Graph;

class Edge
{
	
    Node a;
    Node b;
    /**
     * Please complete the missing tags for Edge
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    Edge( Node _a, Node _b )
    {
        a = _a;
        b = _b;
    }
    /**
     * Please complete the missing tags for print
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    void print()
    {
        System.out.print( "edge (" );
        a.print();
        System.out.print( ", " );
        b.print();
        System.out.print( ") " );
    }
}
