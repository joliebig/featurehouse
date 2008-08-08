
package Graph; 
import java.util.*; 
 
class  Graph {
	 Vector nodes = new Vector();

	 Vector edges = new Vector();

	 public static void main( String [] args ) { System.out.println( "========= BasicGraph ========" ); Graph g = new Graph(); g.add( new Node( 1 ), new Node( 2 ) ); g.add( new Node( 3 ), new Node( 4 ) ); g.print(); System.out.println(); }

	 Edge add( Node n, Node m ) { Edge e = new Edge( n, m ); nodes.add( n ); nodes.add( m ); edges.add( e ); return e; }

	 void print() { for( int i = 0; i < edges.size(); i++ ) { ( ( Edge ) edges.get( i ) ).print(); if( i < edges.size() - 1 ) System.out.print( ", " ); } }


}
