package GPL;

import java.util.LinkedList;
import java.util.Iterator;
import java.util.Collections;
import java.util.Comparator; //dja: add for performance reasons
import java.util.HashMap;
import java.util.Map;
import java.lang.Integer;

public class Graph {
    private LinkedList vertices;

    private LinkedList edges;

    public static final boolean isDirected = false;

    private Map verticesMap;

    public Graph() {
	vertices = new LinkedList();
	edges = new LinkedList();
	verticesMap = new HashMap();
    }

    public void run__wrappee__DFS(Vertex s) {
    }

    public void run__wrappee__MSTKruskal(Vertex s) {
	System.out.println("MSTKruskal");
	Graph gaux = Kruskal();
	gaux.display();
	run__wrappee__DFS(s);
    }

    public void run__wrappee__Cycle(Vertex s) {
	System.out.println("Cycle? " + CycleCheck());
	run__wrappee__MSTKruskal(s);
    }

    public void run__wrappee__Connected(Vertex s) {
	System.out.println("Connected");
	ConnectedComponents();
	run__wrappee__Cycle(s);
    }

    public void run(Vertex s) {
	System.out.println("Number");
	NumberVertices();
	run__wrappee__Connected(s);
    }

    public void sortEdges(Comparator c) {
	Collections.sort(edges, c);
    }

    public void sortVertices(Comparator c) {
	Collections.sort(vertices, c);
    }

    public EdgeIfc addEdge(Vertex start, Vertex end) {
	Edge theEdge = new Edge();
	theEdge.EdgeConstructor(start, end);
	edges.add(theEdge);
	start.addNeighbor(new Neighbor(end, theEdge));
	end.addNeighbor(new Neighbor(start, theEdge));

	start.setDegree(start.getDegree() + 1);
	end.setDegree(end.getDegree() + 1);

	return theEdge;
    }

    // TODO mod
    public Graph getEdgeGraph() {
	Graph edgeGraph = new Graph();
	EdgeIter eIt = getEdges();
	while (eIt.hasNext()) {
	    EdgeIfc e = eIt.next();	    
	    Vertex edgeVertex = new Vertex();
	    edgeVertex.name = e.getStart().getName() + " "
		    + e.getEnd().getName();
	    edgeGraph.addVertex(edgeVertex);
	}

	// add new edges
	EdgeIter eIt1 = getEdges();
	while (eIt1.hasNext()) {
	    EdgeIfc e1 = eIt1.next();
	    EdgeIter eIt2 = getEdges();
	    while (eIt2.hasNext()) {
		EdgeIfc e2 = eIt2.next();
		if (!(e1.getStart().equals(e2.getStart()) && e1.getEnd()
			.equals(e2.getEnd()))
			&& (e1.getStart().equals(e2.getStart())
				|| e1.getStart().equals(e2.getEnd())
				|| e1.getEnd().equals(e2.getStart()) || e1
				.getEnd().equals(e2.getEnd()))) {

		    if (edgeGraph.findsEdge(edgeGraph.findsVertex(e2.getStart()
			    .getName()
			    + " " + e2.getEnd().getName()), edgeGraph
			    .findsVertex((e1.getStart().getName() + " " + e1
				    .getEnd().getName()))) == null)
			edgeGraph.addEdge(edgeGraph.findsVertex((e1.getStart()
				.getName()
				+ " " + e1.getEnd().getName())), edgeGraph
				.findsVertex(e2.getStart().getName() + " "
					+ e2.getEnd().getName()));

		}
	    }
	}
	return edgeGraph;
    }

    public void edgeColoring() {
	Graph edgeGraph = getEdgeGraph();
	edgeGraph.sortVertices(new DescendingDegreeComparator());
	VertexIter vIt;
	Vertex v;
	boolean actionFlag = false;
	int color = 1;
	while (true) {
	    vIt = edgeGraph.getVertices();	    
	    while (vIt.hasNext()) {
		v = vIt.next();		
		if (v.getColor() == 0) {
		    VertexIter neigborVIt = v.getNeighbors();
		    Vertex neighbor;
		    boolean neighborAlreadyHasCurrentColor = false;
		    while (neigborVIt.hasNext()) {			
			neighbor = neigborVIt.next();
			if (neighbor.getColor() == color) {
			    neighborAlreadyHasCurrentColor = true;
			    break;
			}
		    }
		    if (!neighborAlreadyHasCurrentColor) {			
			v.setColor(color);
			actionFlag = true;
		    }
		}
	    }
	    if (!actionFlag) {		
		break;
	    }
	    color++;
	    actionFlag = false;
	}

	EdgeIfc e;
	EdgeIter eIt = getEdges();
	while (eIt.hasNext()) {
	    e = eIt.next();
	    e.setColor(edgeGraph.findsVertex(
		    e.getStart().getName() + " " + e.getEnd().getName())
		    .getColor());
	}
    }

    // TODO /mod

    protected void addVertex(Vertex v) {
	vertices.add(v);
	verticesMap.put(v.name, v);
    }

    public Vertex findsVertex(String theName) {
	Vertex theVertex;
	if (theName == null)
	    return null;
	return (Vertex) verticesMap.get(theName);
    }

    public VertexIter getVertices() {
	return new VertexIter() {
	    private Iterator iter = vertices.iterator();

	    public Vertex next() {
		return (Vertex) iter.next();
	    }

	    public boolean hasNext() {
		return iter.hasNext();
	    }
	};
    }

    public EdgeIter getEdges() {
	return new EdgeIter() {
	    private Iterator iter = edges.iterator();

	    public EdgeIfc next() {
		return (EdgeIfc) iter.next();
	    }

	    public boolean hasNext() {
		return iter.hasNext();
	    }
	};
    }

    public EdgeIfc findsEdge(Vertex theSource, Vertex theTarget) {
	EdgeIfc theEdge;
	for (EdgeIter edgeiter = theSource.getEdges(); edgeiter.hasNext();) {
	    theEdge = edgeiter.next();
	    if ((theEdge.getStart().getName().equals(theSource.getName()) && theEdge
		    .getEnd().getName().equals(theTarget.getName()))
		    || (theEdge.getStart().getName()
			    .equals(theTarget.getName()) && theEdge.getEnd()
			    .getName().equals(theSource.getName())))
		return theEdge;
	}
	return null;
    }

    public void display() {
	System.out.println("******************************************");
	System.out.println("Vertices ");
	for (VertexIter vxiter = getVertices(); vxiter.hasNext();)
	    vxiter.next().display();
	System.out.println("******************************************");
	System.out.println("Edges ");
	for (EdgeIter edgeiter = getEdges(); edgeiter.hasNext();)
	    edgeiter.next().display();
	System.out.println("******************************************");
    }

    public void GraphSearch(WorkSpace w) {
	VertexIter vxiter = getVertices();
	if (vxiter.hasNext() == false) {
	    return;
	}
	while (vxiter.hasNext()) {
	    Vertex v = vxiter.next();
	    v.init_vertex(w);
	}
	for (vxiter = getVertices(); vxiter.hasNext();) {
	    Vertex v = vxiter.next();
	    if (!v.visited) {
		w.nextRegionAction(v);
		v.nodeSearch(w);
	    }
	}
    }

    public Graph Kruskal() {
	LinkedList A = new LinkedList();
	for (VertexIter vxiter = getVertices(); vxiter.hasNext();) {
	    Vertex v = vxiter.next();
	    v.representative = v;
	    v.members = new LinkedList();
	}
	LinkedList Vneighbors = new LinkedList();
	sortEdges(new Comparator() {
	    public int compare(Object o1, Object o2) {
		Edge e1 = (Edge) o1;
		Edge e2 = (Edge) o2;
		if (e1.getWeight() < e2.getWeight())
		    return -1;
		if (e1.getWeight() == e2.getWeight())
		    return 0;
		return 1;
	    }
	});
	Vertex vaux, urep, vrep;
	for (EdgeIter edgeiter = getEdges(); edgeiter.hasNext();) {
	    EdgeIfc e1 = edgeiter.next();
	    Vertex u = e1.getStart();
	    Vertex v = e1.getEnd();
	    if (!(v.representative.getName())
		    .equals(u.representative.getName())) {
		A.add(e1);
		urep = u.representative;
		vrep = v.representative;
		if ((urep.members).size() > (vrep.members).size()) {
		    for (int j = 0; j < (vrep.members).size(); j++) {
			vaux = (Vertex) (vrep.members).get(j);
			vaux.representative = urep;
			(urep.members).add(vaux);
		    }
		    v.representative = urep;
		    vrep.representative = urep;
		    (urep.members).add(v);
		    if (!v.equals(vrep))
			(urep.members).add(vrep);
		    (vrep.members).clear();
		} else {
		    for (int j = 0; j < (urep.members).size(); j++) {
			vaux = (Vertex) (urep.members).get(j);
			vaux.representative = vrep;
			(vrep.members).add(vaux);
		    }
		    u.representative = vrep;
		    urep.representative = vrep;
		    (vrep.members).add(u);
		    if (!u.equals(urep))
			(vrep.members).add(urep);
		    (urep.members).clear();
		}
	    }
	}
	String theName;
	Graph newGraph = new Graph();
	for (VertexIter vxiter = getVertices(); vxiter.hasNext();) {
	    theName = vxiter.next().getName();
	    newGraph.addVertex(new Vertex().assignName(theName));
	}
	Vertex theStart, theEnd;
	Vertex theNewStart, theNewEnd;
	EdgeIfc theEdge;
	for (int i = 0; i < A.size(); i++) {
	    theEdge = (Edge) A.get(i);
	    theStart = theEdge.getStart();
	    theEnd = theEdge.getEnd();
	    theNewStart = newGraph.findsVertex(theStart.getName());
	    theNewEnd = newGraph.findsVertex(theEnd.getName());
	    Edge theNewEdge = (Edge) newGraph.addEdge(theNewStart, theNewEnd);
	    theNewEdge.adjustAdorns((Edge) theEdge);
	}
	return newGraph;
    }

    public boolean CycleCheck() {
	CycleWorkSpace c = new CycleWorkSpace(isDirected);
	GraphSearch(c);
	return c.AnyCycles;
    }

    public void ConnectedComponents() {
	GraphSearch(new RegionWorkSpace());
    }

    public void NumberVertices() {
	GraphSearch(new NumberWorkSpace());
    }

}
