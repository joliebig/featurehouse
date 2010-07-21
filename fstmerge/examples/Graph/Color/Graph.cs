class Graph : X, Y {
    List<Node> nodes = new List<Node>();
    List<Edge> edges = new List<Edge>();
    public Edge Add(Node n, Node m) {
        Edge e = new Edge(n, m);
        nodes.Add(n); nodes.Add(m);
        edges.Add(e);
        return e;
    }
    public void Print() {
        for(int i = 0; i<edges.Count; i++) {
            ((Edge)edges[i]).Print();
            if(i < edges.Count -1)
	            System.Console.Out.Write(", ");
        }
    }
    public static void Main(string[] args) {
  		System.Console.Out.WriteLine("========= BasicGraph ========");
        Graph g = new Graph();
        g.Add(new Node(1), new Node(2));
        g.Add(new Node(3), new Node(4));
        g.Print();
        System.Console.Out.WriteLine();
		System.Console.Out.WriteLine("========= ColorGraph ========");
        Graph g = new Graph();
        g.Add(new Node(5), new Node(6));
        g.Add(new Node(7), new Node(8));
        g.Print();
        System.Console.Out.WriteLine();
    }
}