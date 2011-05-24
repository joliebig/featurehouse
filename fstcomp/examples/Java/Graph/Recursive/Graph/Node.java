// $Header: /home/apel/cvs/fstcomp/examples/Java/Graph/Recursive/Graph/Node.java,v 1.1 2010-03-29 20:44:21 apel Exp $
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
