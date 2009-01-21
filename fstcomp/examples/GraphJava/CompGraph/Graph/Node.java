
package Graph; 
 
class  Node {
	 int id = 0;

	 Node( int _id ) { id = _id; }

	 void print__wrappee__BasicGraph() { System.out.print( id ); }

	 void print__wrappee__Weight() { print__wrappee__BasicGraph(); if( childGraph != null ) { System.out.print( " ++ " ); childGraph.print(); System.out.print( " -- " ); } }

	 void print() { Color.setDisplayColor( color ); print__wrappee__Weight(); }

	 private Graph childGraph = null;

	 Graph getChildGraph() { return childGraph; }

	 void setChildGraph( Graph g ) { childGraph = g; }

	 Color color = new Color( 815 );


}
