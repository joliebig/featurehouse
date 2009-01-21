
package Graph; 
import java.util.*; 
 
class  Graph {
	 Vector nodes = new Vector();

	 Vector edges = new Vector();

	 public static void main__wrappee__BasicGraph( String [] args ) { System.out.println( "========= BasicGraph ========" ); Graph g = new Graph(); g.add( new Node( 1 ), new Node( 2 ) ); g.add( new Node( 3 ), new Node( 4 ) ); g.print(); System.out.println(); }

	 public static void main__wrappee__Recursive( String [] args ) { main__wrappee__BasicGraph( args ); System.out.println( "========= SubGraph ========" ); Graph g = new Graph(); Graph g1 = new Graph(); g1.add( new Node( 11 ), new Node( 12 ) ); Graph g2 = new Graph(); g2.add( new Node( 21 ), new Node( 22 ) ); Graph g3 = new Graph(); g3.add( new Node( 31 ), new Node( 32 ) ); Graph g4 = new Graph(); Node n41 = new Node( 41 ); g4.add( n41, new Node( 42 ) ); Graph g5 = new Graph(); g5.add( new Node( 51 ), new Node( 52 ) ); n41.setChildGraph( g5 ); Node n1 = new Node( 1 ); n1.setChildGraph( g1 ); Node n2 = new Node( 2 ); n2.setChildGraph( g2 ); Node n3 = new Node( 3 ); n3.setChildGraph( g3 ); Node n4 = new Node( 4 ); n4.setChildGraph( g4 ); g.add( n1, n2 ); g.add( n3, n4 ); g.print(); System.out.println(); }

	 public static void main__wrappee__Weight( String [] args ) { main__wrappee__Recursive( args ); System.out.println( "========= WeightedGraph ========" ); Graph g = new Graph(); g.add( new Node( 5 ), new Node( 6 ), new Weight( 1000 ) ); g.add( new Node( 7 ), new Node( 8 ), new Weight( 1001 ) ); g.print(); System.out.println(); }

	 public static void main__wrappee__Color( String [] args ) { main__wrappee__Weight( args ); System.out.println( "========= ColorGraph ========" ); Graph g = new Graph(); g.add( new Node( 5 ), new Node( 6 ) ); g.add( new Node( 7 ), new Node( 8 ) ); g.print(); System.out.println(); }

	 public static void main( String [] args ) { main__wrappee__Color( args ); System.out.println( "========= PrintHeader ========" ); System.out.println( "nothing to do here" ); }

	 Edge add( Node n, Node m ) { Edge e = new Edge( n, m ); nodes.add( n ); nodes.add( m ); edges.add( e ); return e; }

	 void print__wrappee__Color() { for( int i = 0; i < edges.size(); i++ ) { ( ( Edge ) edges.get( i ) ).print(); if( i < edges.size() - 1 ) System.out.print( ", " ); } }

	 void print() { s++; if( s == 1 ) { printTopLevelHeader(); } else { printSubLevelHeader(); } print__wrappee__Color(); s--; }

	 Edge add( Node n, Node m, Weight w ) { Edge res = this.add( n, m ); res.setWeight( w ); return res; }

	 static int s = 0;

	 static void printTopLevelHeader() { System.out.print( "top: " ); }

	 static void printSubLevelHeader() { System.out.print( "sub: " ); }


}
