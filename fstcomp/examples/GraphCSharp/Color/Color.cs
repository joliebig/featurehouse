class Color {
	int val = 0;
	public Color(int v) {
		val = v;
	}
    public static void SetDisplayColor(Color c) {
    	System.Console.Out.WriteLine("color changed to: " + c.val);
    }
}