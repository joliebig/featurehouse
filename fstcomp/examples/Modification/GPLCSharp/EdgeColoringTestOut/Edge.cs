
using System.Collections; 
public class  Edge  : Neighbor, EdgeIfc {
	 public  Vertex start;  public  void EdgeConstructor( Vertex the_start, Vertex the_end ) { start = the_start; end = the_end; }  public  void adjustAdorns( EdgeIfc the_edge ) { }  public  void setWeight(int weight) { }  public  int GetWeight( ) { return 0; }  public  Vertex GetOtherVertex( Vertex vertex ) { if( vertex == start ) { return end; } else if(vertex == end) { return start; } else { return null; } }  public  Vertex GetStart( ) { return start; }  public  Vertex GetEnd( ) { return end; } private  int color; public  int getColor() {return color;} public  void setColor(int color) {this.color = color;}  public  void display__wrappee__DFS( ) { System.Console.Out.WriteLine( " start=" + start.GetName() + " end=" + end.GetName( ) ); } public  void display() {System.Console.Out.Write(" color=" + color);display__wrappee__DFS();}
}
