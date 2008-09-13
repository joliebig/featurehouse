package GPL;

import java.util.Iterator;
import java.util.LinkedList;
import java.lang.Integer;
import java.util.Collections;
import java.util.Comparator;

public class Vertex {

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

    /**
     * @return the degree
     */
    public int getDegree() {
	return degree;
    }

    /**
     * @param degree
     *            the degree to set
     */
    public void setDegree(int degree) {
	this.degree = degree;
    }

    private int degree = 0;

    // TODO /mod

    public LinkedList neighbors;

    public String name;

    public Vertex() {
	VertexConstructor();
    }

    public void VertexConstructor__wrappee__WeightedWithEdges() {
	name = null;
	neighbors = new LinkedList();
    }

    public void VertexConstructor() {
	VertexConstructor__wrappee__WeightedWithEdges();
	visited = false;
    }

    public Vertex assignName(String name) {
	this.name = name;
	return (Vertex) this;
    }

    public String getName() {
	return this.name;
    }

    public LinkedList getNeighborsObj() {
	return neighbors;
    }

    public VertexIter getNeighbors() {
	return new VertexIter() {
	    private Iterator iter = neighbors.iterator();

	    public Vertex next() {
		return ((Neighbor) iter.next()).end;
	    }

	    public boolean hasNext() {
		return iter.hasNext();
	    }
	};
    }

    public void display__wrappee__WeightedWithEdges() {
	System.out.print(" Node " + name + " connected to: ");
	for (VertexIter vxiter = getNeighbors(); vxiter.hasNext();) {
	    System.out.print(vxiter.next().getName() + ", ");
	}
	System.out.println();
    }

    public void display__wrappee__DFS() {
	if (visited)
	    System.out.print("  visited");
	else
	    System.out.println(" !visited ");
	display__wrappee__WeightedWithEdges();
    }

    public void display__wrappee__MSTKruskal() {
	if (representative == null)
	    System.out.print("Rep null ");
	else
	    System.out.print(" Rep " + representative.getName() + " ");
	display__wrappee__DFS();
    }

    public void display__wrappee__Cycle() {
	System.out.print(" VertexCycle# " + VertexCycle + " ");
	display__wrappee__MSTKruskal();
    }

    public void display__wrappee__Connected() {
	System.out.print(" comp# " + componentNumber + " ");
	display__wrappee__Cycle();
    }

    public void display() {
	System.out.print(" # " + VertexNumber + " ");

	// TODO mod
	System.out.print(" color " + getColor() + " ");
	System.out.print(" Degree " + getDegree() + " ");
	// TODO /mod
	display__wrappee__Connected();
    }

    public void addNeighbor(Neighbor n) {
	neighbors.add(n);
    }

    public EdgeIter getEdges() {
	return new EdgeIter() {
	    private Iterator iter = neighbors.iterator();

	    public EdgeIfc next() {
		return ((EdgeIfc) ((Neighbor) iter.next()).edge);
	    }

	    public boolean hasNext() {
		return iter.hasNext();
	    }
	};
    }

    public boolean visited;

    public void init_vertex(WorkSpace w) {
	visited = false;
	w.init_vertex((Vertex) this);
    }

    public void nodeSearch(WorkSpace w) {
	Vertex v;
	w.preVisitAction((Vertex) this);
	if (visited)
	    return;
	visited = true;
	for (VertexIter vxiter = getNeighbors(); vxiter.hasNext();) {
	    v = vxiter.next();
	    w.checkNeighborAction((Vertex) this, v);
	    v.nodeSearch(w);
	}
	w.postVisitAction((Vertex) this);
    }

    public Vertex representative;

    public LinkedList members;

    public int VertexCycle;

    public int VertexColor;

    public int componentNumber;

    public int VertexNumber;

}
