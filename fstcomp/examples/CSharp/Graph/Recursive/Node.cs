//refines class Node {
class Node {
	private Graph childGraph = null;
	public void SetChildGraph(Graph g) {
		childGraph = g;
	}
    Graph GetChildGraph() {
        return childGraph;
    }
    public void Print() {
		original();
		if(childGraph != null) {
            System.Console.Out.Write(" ++ ");
            childGraph.Print();
            System.Console.Out.Write(" -- ");
		}
    }
}
