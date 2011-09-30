class Statistics {
	public int class_constructor_count = 0;
    public int class_constructor_loc = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("class constructors (noc):\t" + class_constructor_count);
        System.out.println("class constructors (loc):\t" + class_constructor_loc);
    }
}
