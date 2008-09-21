public class Graph {
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
}
