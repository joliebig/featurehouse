class AspectJCollector {
    public void Block() throws ParseException {
        int begin = this.token.beginLine;
        super.Block();
        int end = this.token.endLine;
        
        if(inter_type_constructor_flag && nested_block_count == 0) {
	        stats.inter_type_constructor_count++;
    	    stats.inter_type_constructor_loc += end - begin + 1;  
    	}
    }
}
