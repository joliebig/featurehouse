//refines class Graph {
class Graph {
    static int s = 0;
    public void Print() {
        s++;
        if(s == 1) {
           PrintTopLevelHeader();
        } else {
            PrintSubLevelHeader();
        }
        original();
        s--;
    }
    static void PrintTopLevelHeader() { 
        System.Console.Out.Write("top: ");
    }
    static void PrintSubLevelHeader() {
        System.Console.Out.Write("sub: ");
    }
    public static void Main(string[] args) {
    	original(args);
		System.Console.Out.WriteLine("========= PrintHeader ========");
        System.Console.Out.WriteLine("nothing to do here");
        System.Console.In.Read();
    }
}
