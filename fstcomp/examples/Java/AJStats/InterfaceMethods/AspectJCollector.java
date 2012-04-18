class AspectJCollector {
    public void MethodDeclaration() throws ParseException {
        int begin = this.token.beginLine;
        super.MethodDeclaration();
        int end = this.token.endLine;
        
        if(interface_flag) {
	        stats.interface_method_count++;
    	}
    }
}
