
public interface  EdgeIfc {
	 Vertex GetStart( );  Vertex GetEnd( );  void display( );  void setWeight( int weight );  Vertex GetOtherVertex( Vertex vertex );  void adjustAdorns( EdgeIfc the_edge );
}
