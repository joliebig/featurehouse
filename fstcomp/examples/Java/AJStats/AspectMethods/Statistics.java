class Statistics {
	public int aspect_method_count = 0;
    public int aspect_method_loc = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("aspect methods (noc):\t\t" + aspect_method_count);
        System.out.println("aspect methods (loc):\t\t" + aspect_method_loc);
    }
}
