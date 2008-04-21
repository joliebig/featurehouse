//refines class Edge {
class Edge {
	Weight w = new Weight(0);
    public void SetWeight(Weight _w) { 
    	w = _w; 
    }
    public void Print() {
        original(); 
        System.Console.Out.Write(" ["); 
        w.Print();
        System.Console.Out.Write("] "); 
    }
}
