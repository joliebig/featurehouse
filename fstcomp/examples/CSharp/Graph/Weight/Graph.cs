//refines class Graph {
class Graph {
    Edge Add(Node n, Node m, Weight w) {
        Edge res = this.Add(n, m);
        res.SetWeight(w);
        return res;
    }
    
    public static void Main(string[] args) {
    	original(args);
		System.Console.Out.WriteLine("========= WeightedGraph ========");
        Graph g = new Graph();
        g.Add(new Node(5), new Node(6), new Weight(1000));
        g.Add(new Node(7), new Node(8), new Weight(1001));
        g.Print();
        System.Console.Out.WriteLine();
    }
}
