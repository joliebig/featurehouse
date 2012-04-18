class Statistics {
	public int inter_type_method_count = 0;
    public int inter_type_method_loc = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("inter-type methods (noc):\t" + inter_type_method_count);
        System.out.println("inter-type methods (loc):\t" + inter_type_method_loc);
    }
}
