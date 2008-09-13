
package Graph; 
import java.util.*; import java.util.List; 
 
class  Graph {
	 Vector nodes = new Vector();

	 Vector edges = new Vector();

	 public static void main( String [] args ) { System.out.println( "========= BasicGraph ========" ); Graph g = new Graph(); g.add( new Node( 1 ), new Node( 2 ) ); g.add( new Node( 3 ), new Node( 4 ) ); g.print(); System.out.println(); }

	 Edge add( Node n, Node m ) { Edge e = new Edge( n, m ); nodes.add( n ); nodes.add( m ); edges.add( e ); return e; }

	public void foo(){System.out.println("bäm");}

	 void print() {	System.out.println("new print method from Bar"); }


}
