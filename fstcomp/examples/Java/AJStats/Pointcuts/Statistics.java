class Statistics {
	public int pointcut_count = 0;
    
    public void print() {
    	original();
    	System.out.println("---");
        System.out.println("pointcuts (noc):\t\t" + pointcut_count);
    }
}
