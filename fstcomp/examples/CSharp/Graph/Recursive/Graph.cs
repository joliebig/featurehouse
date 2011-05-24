//refines class Graph {
class Graph {
    public static void Main(string[] args) {
    	original(args);
  		System.Console.Out.WriteLine("========= SubGraph ========");
        Graph g = new Graph();
        Graph g1 = new Graph();
        g1.Add(new Node(11), new Node(12));
        Graph g2 = new Graph();
        g2.Add(new Node(21), new Node(22));
        Graph g3 = new Graph();
        g3.Add(new Node(31), new Node(32));
        Graph g4 = new Graph();
        Node n41 = new Node(41);
        g4.Add(n41, new Node(42));
        
        Graph g5 = new Graph();
        g5.Add(new Node(51), new Node(52));
        n41.SetChildGraph(g5);
        
        Node n1 = new Node(1);
        n1.SetChildGraph(g1);
        Node n2 = new Node(2);
        n2.SetChildGraph(g2);
        Node n3 = new Node(3);
        n3.SetChildGraph(g3);
        Node n4 = new Node(4);
        n4.SetChildGraph(g4);

        g.Add(n1, n2);
        g.Add(n3, n4);
        g.Print();
        System.Console.Out.WriteLine();
    }
}
