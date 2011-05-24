public class Graph {
    public void edgeColoring() {
	Graph edgeGraph = getEdgeGraph();
	edgeGraph.sortVertices(new DescendingDegreeComparator());	
	Iterator<Vertex> vIt;
	Vertex v;
	bool actionFlag = false;
	int color = 1;
	while (true) {
	     vIt = edgeGraph.GetVertices();	    
	    while (vIt.hasNext()) {
		v = vIt.next(); 
		if (v.getColor() == 0) {		    
		     Iterator<Vertex>  neigborVIt = v.GetNeighbors();
		    Vertex neighbor;
		    bool neighborAlreadyHasCurrentColor = false;
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

	foreach (EdgeIfc e in edges) {		   
	    e.setColor(edgeGraph.findsVertex(
		    e.GetStart().GetName() + " " + e.GetEnd().GetName())
		    .getColor());
	}
    }    
}
