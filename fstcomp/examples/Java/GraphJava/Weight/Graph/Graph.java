// $Header: /home/apel/cvs/fstcomp/examples/Java/GraphJava/Weight/Graph/Attic/Graph.java,v 1.1 2010-03-29 20:22:08 apel Exp $
/**
 * Please complete these missing tags
 * @author
 * @rref
 * @copyright
 * @concurrency
 * @see
 */
package Graph;

class Graph
{
    /**
     * Please complete the missing tags for main
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    
    public static void main( String [] args )
    {
        original( args );
        System.out.println( "========= WeightedGraph ========" );
        Graph g = new Graph();
        g.add( new Node( 5 ), new Node( 6 ), new Weight( 1000 ) );
        g.add( new Node( 7 ), new Node( 8 ), new Weight( 1001 ) );
        g.print();
        System.out.println();
    }
    /**
     * Please complete the missing tags for add
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    Edge add( Node n, Node m, Weight w )
    {
        Edge res = this.add( n, m );
        res.setWeight( w );
        return res;
    }
}
