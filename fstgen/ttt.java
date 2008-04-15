
package Graph;  
class  Edge  extends Object {
	 Node a, b;

	 Edge( Node _a, Node _b ) { a = _a; b = _b; }

	 void print() { original(); System.out.print( " [" ); w.print(); System.out.print( "] " ); }

	 Weight w = new Weight( 0 );

	 void setWeight( Weight _w ) { w = _w; }


