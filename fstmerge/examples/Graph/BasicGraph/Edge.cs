class Edge {
	Node a, b;
	public Edge(Node _a, Node _b) { a = _a; b = _b; }
    public void Print() {
        System.Console.Out.Write("edge ("); 
        a.Print(); 
        System.Console.Out.Write(", ");
        b.Print();
        System.Console.Out.Write(") "); 
    }
}