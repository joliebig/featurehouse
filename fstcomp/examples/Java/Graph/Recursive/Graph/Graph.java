// $Header: /home/apel/cvs/fstcomp/examples/Java/Graph/Recursive/Graph/Graph.java,v 1.1 2010-03-29 20:44:21 apel Exp $
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
        System.out.println( "========= SubGraph ========" );
        Graph g = new Graph();
        Graph g1 = new Graph();
        g1.add( new Node( 11 ), new Node( 12 ) );
        Graph g2 = new Graph();
        g2.add( new Node( 21 ), new Node( 22 ) );
        Graph g3 = new Graph();
        g3.add( new Node( 31 ), new Node( 32 ) );
        Graph g4 = new Graph();
        Node n41 = new Node( 41 );
        g4.add( n41, new Node( 42 ) );
        
        Graph g5 = new Graph();
        g5.add( new Node( 51 ), new Node( 52 ) );
        n41.setChildGraph( g5 );
        
        
        Node n1 = new Node( 1 );
        n1.setChildGraph( g1 );
        Node n2 = new Node( 2 );
        n2.setChildGraph( g2 );
        Node n3 = new Node( 3 );
        n3.setChildGraph( g3 );
        Node n4 = new Node( 4 );
        n4.setChildGraph( g4 );
        
        g.add( n1, n2 );
        g.add( n3, n4 );
        g.print();
        System.out.println();
    }
}
