class AspectJCollector {
    public void VariableDeclarator() throws ParseException {
        int begin = this.token.beginLine;
        super.VariableDeclarator();
        int end = this.token.endLine;

        if(interface_flag && !method_flag) {
	        stats.interface_field_count++;
    	}
    }
}
