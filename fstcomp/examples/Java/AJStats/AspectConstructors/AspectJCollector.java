class AspectJCollector {
	private int constructor_begin = -1;
    public void ConstructorDeclaration() throws ParseException {
		super.ConstructorDeclaration();
		int end = this.token.endLine;
		        
        if(aspect_flag) {
	        stats.aspect_constructor_count++;
       	    stats.aspect_constructor_loc += end - constructor_begin + 1;  
       	    constructor_begin = -1;
    	}
    }
    
    public void FormalParameters() throws ParseException {
	    int begin = this.token.beginLine;
		super.FormalParameters();
        
        if(aspect_flag && constructor_flag) {
        	if(constructor_begin == -1)
				constructor_begin = begin;
    	}
    }
}

