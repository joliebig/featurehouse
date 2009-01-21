
package Graph; 
 
class  Edge {
	 Node a, b;

	 Edge( Node _a, Node _b ) { a = _a; b = _b; }

	 void print__wrappee__Recursive() { System.out.print( "edge (" ); a.print(); System.out.print( ", " ); b.print(); System.out.print( ") " ); }

	 void print__wrappee__Weight() { print__wrappee__Recursive(); System.out.print( " [" ); w.print(); System.out.print( "] " ); }

	 void print() { Color.setDisplayColor( color ); print__wrappee__Weight(); }

	 Weight w = new Weight( 0 );

	 void setWeight( Weight _w ) { w = _w; }

	 Color color = new Color( 4711 );


}
