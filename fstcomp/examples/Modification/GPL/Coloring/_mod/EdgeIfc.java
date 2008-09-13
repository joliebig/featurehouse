package GPL;

public interface EdgeIfc {

    // TODO mod
    public int getColor();

    public void setColor(int color);
    // TODO /mod

    public Vertex getStart();

    public Vertex getEnd();

    public void display();

    public void setWeight(int weight);

    public Vertex getOtherVertex(Vertex vertex);

    public void adjustAdorns(EdgeIfc the_edge);

    public int getWeight();

}
