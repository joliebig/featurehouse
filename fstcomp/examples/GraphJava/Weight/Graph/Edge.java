// $Header: /home/apel/cvs/fstcomp/examples/GraphJava/Weight/Graph/Attic/Edge.java,v 1.1 2008-04-15 21:26:43 apel Exp $
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
    Weight w = new Weight( 0 );
    /**
     * Please complete the missing tags for setWeight
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    void setWeight( Weight _w )
    {
        w = _w;
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
        original();
        System.out.print( " [" );
        w.print();
        System.out.print( "] " );
    }
}
