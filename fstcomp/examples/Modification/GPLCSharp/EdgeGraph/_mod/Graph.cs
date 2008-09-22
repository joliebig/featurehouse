public class Graph {
    public Graph getEdgeGraph() {
	Graph edgeGraph = new Graph();
	
	foreach (EdgeIfc e in edges) {	    
	    Vertex edgeVertex = new Vertex();
	    edgeVertex.name = e.GetStart().GetName() + " "
		    + e.GetEnd().GetName();
	    edgeGraph.AddVertex(edgeVertex);
	}

	// add new edges
	foreach (EdgeIfc e1 in edges) {	 
	   foreach (EdgeIfc e2 in edges) { 
		if (!(e1.GetStart().Equals(e2.GetStart()) && e1.GetEnd()
			.Equals(e2.GetEnd()))
			&& (e1.GetStart().Equals(e2.GetStart())
				|| e1.GetStart().Equals(e2.GetEnd())
				|| e1.GetEnd().Equals(e2.GetStart()) || e1
				.GetEnd().Equals(e2.GetEnd()))) {

		    if (edgeGraph.findsEdge(edgeGraph.findsVertex(e2.GetStart()
			    .GetName()
			    + " " + e2.GetEnd().GetName()), edgeGraph
			    .findsVertex((e1.GetStart().GetName() + " " + e1
				    .GetEnd().GetName()))) == null)
			edgeGraph.AddEdge(edgeGraph.findsVertex((e1.GetStart()
				.GetName()
				+ " " + e1.GetEnd().GetName())), edgeGraph
				.findsVertex(e2.GetStart().GetName() + " "
					+ e2.GetEnd().GetName()));
		}
	    }
	}
	return edgeGraph;
    }
    
}
