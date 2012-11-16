import static org.junit.Assert.*;

import org.junit.*;

public class Test{



/*@
 	requires 0 <= p && p < id.length; 
 	ensures \result == (id[p] == id[q]);
 @*/
public /*@pure@*/ boolean connected(int p, int q) {
    return id[p] == id[q];
}


/*@requires \original == 0;
	      also
	       \original_case
	       
   
 */
public void union(int p, int q) {
    if (connected(p, q)) return;
    int pid = id[p];
    for (int i = 0; i < id.length; i++)
        if (id[i] == pid) id[i] = id[q]; 
    count--;
}


}