class Statistics {
	public int aspect_constructor_count = 0;
    public int aspect_constructor_loc = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("aspect constructors (noc):\t" + aspect_constructor_count);
        System.out.println("aspect constructors (loc):\t" + aspect_constructor_loc);
    }
}
