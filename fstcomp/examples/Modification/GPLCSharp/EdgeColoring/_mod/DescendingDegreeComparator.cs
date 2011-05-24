/**
 * 
 */

/**
 * @author boxleitner
 * 
 */
public class DescendingDegreeComparator : System.Collections.Generic.IComparer<Vertex> {

    public int Compare(Vertex arg0, Vertex arg1) {
	return arg1.getDegree() - arg0.getDegree();
    }

}
