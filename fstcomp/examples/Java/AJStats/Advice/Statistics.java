class Statistics {
	public int advice_count = 0;
    public int advice_loc = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("advice (noc):\t\t\t" + advice_count);
        System.out.println("advice (loc):\t\t\t" + advice_loc);
    }
}
