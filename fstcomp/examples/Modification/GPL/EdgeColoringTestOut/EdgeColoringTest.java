 class  EdgeColoringTest {
	 public static void main(String[] args) {	Graph edgeColorTestGraph = new Graph();	Vertex v1 = new Vertex();	v1.name = "v1";	Vertex v2 = new Vertex();	v2.name = "v2";	Vertex v3 = new Vertex();	v3.name = "v3";	Vertex v4 = new Vertex();	v4.name = "v4";	Vertex v5 = new Vertex();	v5.name = "v5";	edgeColorTestGraph.addVertex(v1);	edgeColorTestGraph.addVertex(v2);	edgeColorTestGraph.addVertex(v3);	edgeColorTestGraph.addVertex(v4);	edgeColorTestGraph.addVertex(v5);	edgeColorTestGraph.addEdge(v1, v2);	edgeColorTestGraph.addEdge(v1, v3);	edgeColorTestGraph.addEdge(v1, v4);	edgeColorTestGraph.addEdge(v2, v5);	edgeColorTestGraph.addEdge(v3, v4);	edgeColorTestGraph.addEdge(v5, v4);	edgeColorTestGraph.display();	edgeColorTestGraph.edgeColoring();	edgeColorTestGraph.display(); }


}
