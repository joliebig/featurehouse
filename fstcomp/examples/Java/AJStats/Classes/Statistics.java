class Statistics {
	public int class_count = 0;
    public int top_class_count = 0;
    public int nested_class_count = 0;
    public int class_loc = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("classes (noc):\t\t\t" + class_count);
        System.out.println("top classes (noc):\t\t" + top_class_count);
        System.out.println("nested classes (noc):\t\t" + nested_class_count);
        System.out.println("classes (loc):\t\t\t" + class_loc);
    }
}
