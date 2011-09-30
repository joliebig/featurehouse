class Statistics {
	public int inter_type_constructor_count = 0;
    public int inter_type_constructor_loc = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("inter-type constructors (noc):\t" + inter_type_constructor_count);
        System.out.println("inter-type constructors (loc):\t" + inter_type_constructor_loc);
    }
}
