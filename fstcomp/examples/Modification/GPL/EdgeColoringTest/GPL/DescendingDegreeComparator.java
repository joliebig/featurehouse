
package GPL; 
import java.util.Comparator; 
public  class  DescendingDegreeComparator  implements Comparator<Vertex> {
	 public int compare(Vertex arg0, Vertex arg1) {	return arg1.getDegree() - arg0.getDegree(); }


}
