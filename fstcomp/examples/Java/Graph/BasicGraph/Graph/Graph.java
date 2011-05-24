// $Header: /home/apel/cvs/fstcomp/examples/Java/Graph/BasicGraph/Graph/Graph.java,v 1.1 2010-03-29 20:44:17 apel Exp $
/**
 * Please complete these missing tags
 * @author
 * @rref
 * @copyright
 * @concurrency
 * @see
 */
package Graph;

import java.util.*;

class Graph
{
    Vector nodes = new Vector();
    Vector edges = new Vector();
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
        System.out.println( "========= BasicGraph ========" );
        Graph g = new Graph();
        g.add( new Node( 1 ), new Node( 2 ) );
        g.add( new Node( 3 ), new Node( 4 ) );
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
    Edge add( Node n, Node m )
    {
        Edge e = new Edge( n, m );
        nodes.add( n );
        nodes.add( m );
        edges.add( e );
        return e;
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
        for( int i = 0; i < edges.size(); i++ )
        {
            ( ( Edge ) edges.get( i ) ).print();
            if( i < edges.size() - 1 )
            System.out.print( ", " );
        }
    }
}
