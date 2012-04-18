class Statistics {
    public int interface_count = 0;
    public int top_interface_count = 0;
    public int nested_interface_count = 0;
    public int interface_loc = 0;
    
    public void print() {
    	original();
        System.out.println("---");        
        System.out.println("interfaces (noc):\t\t" + interface_count);
        System.out.println("top interfaces (noc):\t\t" + top_interface_count);
        System.out.println("nested interfaces (noc):\t" + nested_interface_count);
        System.out.println("interfaces (loc):\t\t" + interface_loc);
    }
}
