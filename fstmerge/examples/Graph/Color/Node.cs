class Node {
    int id = 0;
    Color color = new Color(815);
    public Node(int _id) { id = _id; }
    public void Print() {
    	Color.SetDisplayColor(color);
		System.Console.Out.Write(id);
    }
}