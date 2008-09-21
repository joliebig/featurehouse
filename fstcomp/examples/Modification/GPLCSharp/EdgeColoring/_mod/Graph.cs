public class Graph {
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
}
