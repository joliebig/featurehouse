/**
 * 
 */
package GPL;

import java.util.Comparator;

/**
 * @author boxleitner
 * 
 */
public class DescendingDegreeComparator implements Comparator<Vertex> {

    public int compare(Vertex arg0, Vertex arg1) {
	return arg1.getDegree() - arg0.getDegree();
    }

}
