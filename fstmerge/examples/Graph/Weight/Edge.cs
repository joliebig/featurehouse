class Edge {
	Node a, b;
	Weight w = new Weight(0);
	public Edge(Node _a, Node _b) { a = _a; b = _b; }
    public void SetWeight(Weight _w) { 
    	w = _w; 
    }
    public void Print() {
        System.Console.Out.Write("edge ("); 
        a.Print(); 
        System.Console.Out.Write(", ");
        b.Print();
        System.Console.Out.Write(") "); 
        System.Console.Out.Write(" ["); 
        w.Print();
        System.Console.Out.Write("] "); 
    }
}