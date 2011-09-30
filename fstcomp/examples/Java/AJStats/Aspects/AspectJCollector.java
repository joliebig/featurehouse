class AspectJCollector {
    public void AspectBody() throws ParseException {
        int begin = this.token.beginLine;
        super.AspectBody();
        int end = this.token.endLine;
        stats.aspect_loc += end - begin + 1;
    }

    public void UnmodifiedAspectDeclaration() throws ParseException {
        super.UnmodifiedAspectDeclaration();
        stats.aspect_count++;
    }
    
    public void AspectDeclaration() throws ParseException {
        super.AspectDeclaration();
        stats.top_aspect_count++;        
    }

    public void NestedAspectDeclaration() throws ParseException {
        super.NestedAspectDeclaration();
        stats.nested_aspect_count++;        
    }
}
