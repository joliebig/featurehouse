class AspectJCollector {
    public void VariableDeclarator() throws ParseException {
        int begin = this.token.beginLine;
        super.VariableDeclarator();
        int end = this.token.endLine;

        if(class_flag && !method_flag) {
	        stats.class_field_count++;
    	}
    }
}
