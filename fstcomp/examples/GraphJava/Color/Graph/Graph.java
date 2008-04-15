// $Header: /home/apel/cvs/fstcomp/examples/GraphJava/Color/Graph/Attic/Graph.java,v 1.1 2008-04-15 21:26:42 apel Exp $
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
        System.out.println( "========= ColorGraph ========" );
        Graph g = new Graph();
        g.add( new Node( 5 ), new Node( 6 ) );
        g.add( new Node( 7 ), new Node( 8 ) );
        g.print();
        System.out.println();
    }
}
