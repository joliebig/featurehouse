class AspectJCollector {
    public void Block() throws ParseException {
        int begin = this.token.beginLine;
        super.Block();
        int end = this.token.endLine;
        
        if(method_flag && aspect_flag && nested_block_count == 0) {
	        stats.aspect_method_count++;
    	    stats.aspect_method_loc += end - begin + 1;  
    	}
    }
}
