class Statistics {
	public int class_method_count = 0;
    public int class_method_loc = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("class methods (noc):\t\t" + class_method_count);
        System.out.println("class methods (loc):\t\t" + class_method_loc);
    }
}
