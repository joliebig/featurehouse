class Edge {
	Node a, b;
	Color color = new Color(4711);
	public Edge(Node _a, Node _b) { a = _a; b = _b; }
    public void Print() {
		Color.SetDisplayColor(color);
		System.Console.Out.Write("edge ("); 
        a.Print(); 
        System.Console.Out.Write(", ");
        b.Print();
        System.Console.Out.Write(") "); 
    }
}