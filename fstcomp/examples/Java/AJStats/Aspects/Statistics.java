class Statistics {
	public int aspect_count = 0;
    public int top_aspect_count = 0;
    public int nested_aspect_count = 0;
    public int aspect_loc = 0;
    
    public void print() {
		original();
        System.out.println("---");
        System.out.println("aspects (noc):\t\t\t" + aspect_count);
        System.out.println("top aspects (noc):\t\t" + top_aspect_count);
        System.out.println("nested aspects (noc):\t\t" + nested_aspect_count);
        System.out.println("aspects (loc):\t\t\t" + aspect_loc);
    }
    
}
