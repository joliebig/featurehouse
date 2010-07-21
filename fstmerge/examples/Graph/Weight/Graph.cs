using System.Collections.Generic;
class Graph : X, Z {
    List<Node> nodes = new List<Node>();
    List<Edge> edges = new List<Edge>();
    public Edge Add(Node n, Node m) {
        Edge e = new Edge(n, m);
        nodes.Add(n); nodes.Add(m);
        edges.Add(e);
        return e;
    }
    Edge Add(Node n, Node m, Weight w) {
        Edge res = this.Add(n, m);
        res.SetWeight(w);
        return res;
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
        System.Console.Out.WriteLine("========= WeightedGraph ========");
        Graph g = new Graph();
        g.Add(new Node(5), new Node(6), new Weight(1000));
        g.Add(new Node(7), new Node(8), new Weight(1001));
        g.Print();
        System.Console.Out.WriteLine();
    }
}