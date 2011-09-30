class AspectJCollector {
    public void Block() throws ParseException {
        int begin = this.token.beginLine;
        super.Block();
        int end = this.token.endLine;
        
        if(advice_flag && nested_block_count == 0) {
	        stats.advice_count++;
    	    stats.advice_loc += end - begin + 1;  
    	}
    }
}
