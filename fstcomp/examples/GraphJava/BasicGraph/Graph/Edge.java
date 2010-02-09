// $Header: /home/apel/cvs/fstcomp/examples/GraphJava/BasicGraph/Graph/Attic/Edge.java,v 1.2 2010-02-09 19:40:34 apel Exp $
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
    Node a,c;
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
