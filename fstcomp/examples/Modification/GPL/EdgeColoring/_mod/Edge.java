package GPL;

import java.util.LinkedList;

public class Edge extends Neighbor implements EdgeIfc {

    // TODO mod
    private int color;

    /**
     * @return the color
     */
    public int getColor() {
	return color;
    }

    /**
     * @param color
     *            the color to set
     */
    public void setColor(int color) {
	this.color = color;
    }

    // TODO /mod
    public Vertex start;

    public void EdgeConstructor(Vertex the_start, Vertex the_end) {
	start = the_start;
	end = the_end;
    }

    public void adjustAdorns__wrappee__UndirectedWithEdges(EdgeIfc the_edge) {
    }

    public void adjustAdorns(EdgeIfc the_edge) {
	setWeight(the_edge.getWeight());
	adjustAdorns__wrappee__UndirectedWithEdges(the_edge);
    }

    public void setWeight(int weight) {
	this.weight = weight;
    }

    public int getWeight() {
	return this.weight;
    }

    public Vertex getOtherVertex(Vertex vertex) {
	if (vertex == start) {
	    return end;
	} else if (vertex == end) {
	    return start;
	} else {
	    return null;
	}
    }

    public Vertex getStart() {
	return start;
    }

    public Vertex getEnd() {
	return end;
    }

    public void display__wrappee__UndirectedWithEdges() {
	System.out.println(" start=" + start.getName() + " end="
		+ end.getName());
    }

    public void display() {
	System.out.print(" Weight=" + weight);
	// TODO mod
	System.out.print(" color=" + color);
	// TODO /mod
	display__wrappee__UndirectedWithEdges();
    }

    private int weight;

    public void EdgeConstructor(Vertex the_start, Vertex the_end, int the_weight) {
	EdgeConstructor(the_start, the_end);
	weight = the_weight;
    }

}
