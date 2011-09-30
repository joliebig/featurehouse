class Statistics {
	public int inter_type_declare_count = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("inter-type declares (noc):\t" + inter_type_declare_count);
    }
}
