class AspectJCollector {
    public void ClassBody() throws ParseException {
        int begin = this.token.beginLine;
        super.ClassBody();
        int end = this.token.endLine;
        stats.class_loc += end - begin + 1;      
    }

    public void UnmodifiedClassDeclaration() throws ParseException {
        super.UnmodifiedClassDeclaration();
        stats.class_count++;        
    }
    
    public void ClassDeclaration() throws ParseException {
        super.ClassDeclaration();
        stats.top_class_count++;        
    }

    public void NestedClassDeclaration() throws ParseException {
        super.NestedClassDeclaration();
        stats.nested_class_count++;        
    }
}
