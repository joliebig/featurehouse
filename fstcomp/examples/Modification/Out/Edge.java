
package Graph; 
 
class  Edge {
	 Node a, b;

	 Edge( Node _a, Node _b ) { a = _a; b = _b; }

	 void print() { System.out.print( "edge (" ); a.print(); System.out.print( ", " ); b.print(); System.out.print( ") " ); }


}
