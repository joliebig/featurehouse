// $Header: /home/apel/cvs/fstcomp/examples/GraphJava/PrintHeader/Graph/Attic/Graph.java,v 1.1 2008-04-15 21:26:43 apel Exp $
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
    static int s = 0;
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
        System.out.println( "========= PrintHeader ========" );
        System.out.println( "nothing to do here" );
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
        s++;
        if( s == 1 )
        {
            printTopLevelHeader();
        }
        else
        {
            printSubLevelHeader();
        }
        original();
        s--;
    }
    /**
     * Please complete the missing tags for printTopLevelHeader
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    static void printTopLevelHeader()
    {
        System.out.print( "top: " );
    }
    /**
     * Please complete the missing tags for printSubLevelHeader
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    static void printSubLevelHeader()
    {
        System.out.print( "sub: " );
    }
}
