
package GPL; 
public  interface  EdgeIfc {
	 public Vertex getStart( );

	 public Vertex getEnd( );

	 public void display( );

	 public void setWeight( int weight );

	 public Vertex getOtherVertex( Vertex vertex );

	 public void adjustAdorns( EdgeIfc the_edge );

	public int getColor();

	public void setColor(int color);


}
