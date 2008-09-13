package GPL; 
public  class  VertexIter {
	 public boolean hasNext( ) { return false; }

	 public Vertex next( ) { return null; }

	private int degree;

	public int getDegree() {return degree;}

	public void setDegree(int degree) {this.degree =	degree;}

	private int color;

	public int getColor() {return color;}

	public void setColor(int color) {this.color = color;}


}
