class AspectJCollector {
	private int constructor_begin = -1;
    public void ConstructorDeclaration() throws ParseException {
		super.ConstructorDeclaration();
		int end = this.token.endLine;
		        
        if(class_flag) {
	        stats.class_constructor_count++;
       	    stats.class_constructor_loc += end - constructor_begin + 1;  
       	    constructor_begin = -1;
    	}
    }
    
    public void FormalParameters() throws ParseException {
	    int begin = this.token.beginLine;
		super.FormalParameters();
        
        if(class_flag && constructor_flag) {
        	if(constructor_begin == -1)
				constructor_begin = begin;
    	}
    }
}
