// $Header: /home/apel/cvs/fstcomp/examples/Java/Graph/Weight/Graph/Edge.java,v 1.1 2010-03-29 20:44:20 apel Exp $
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
