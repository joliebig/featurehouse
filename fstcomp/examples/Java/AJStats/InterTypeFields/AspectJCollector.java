class AspectJCollector {
    public void AspectVariableDeclarator() throws ParseException {
        int begin = this.token.beginLine;
        super.AspectVariableDeclarator();
        int end = this.token.endLine;
        
        if(inter_type_field_flag) {
	        stats.inter_type_field_count++;
            inter_type_field_flag = false;
    	}
    }
}
