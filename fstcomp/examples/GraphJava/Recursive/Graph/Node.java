// $Header: /home/apel/cvs/fstcomp/examples/GraphJava/Recursive/Graph/Attic/Node.java,v 1.1 2008-04-15 21:26:43 apel Exp $
/**
 * Please complete these missing tags
 * @author
 * @rref
 * @copyright
 * @concurrency
 * @see
 */
package Graph;

class Node
{
    private Graph childGraph = null;
    /**
     * Please complete the missing tags for getChildGraph
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    Graph getChildGraph()
    {
        return childGraph;
    }
    /**
     * Please complete the missing tags for setChildGraph
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    void setChildGraph( Graph g )
    {
        childGraph = g;
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
        if( childGraph != null )
        {
            System.out.print( " ++ " );
            childGraph.print();
            System.out.print( " -- " );
        }
    }
}
