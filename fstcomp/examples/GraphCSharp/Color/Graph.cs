//refines class Graph {
class Graph {
    public static void Main(string[] args) {
    	original(args);
		System.Console.Out.WriteLine("========= ColorGraph ========");
        Graph g = new Graph();
        g.Add(new Node(5), new Node(6));
        g.Add(new Node(7), new Node(8));
        g.Print();
        System.Console.Out.WriteLine();
    }
}
