// $Header: /home/apel/cvs/fstcomp/examples/GraphJava/Color/Graph/Attic/Color.java,v 1.1 2008-04-15 21:26:42 apel Exp $
/**
 * Please complete these missing tags
 * @author
 * @rref
 * @copyright
 * @concurrency
 * @see
 */
package Graph;

class Color
{
    int val = 0;
    /**
     * Please complete the missing tags for Color
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    Color( int v )
    {
        val = v;
    }
    /**
     * Please complete the missing tags for setDisplayColor
     * @param
     * @return
     * @throws
     * @pre
     * @post
     */
    static void setDisplayColor( Color c )
    {
        System.out.println( "color changed to: " + c.val );
    }
}
