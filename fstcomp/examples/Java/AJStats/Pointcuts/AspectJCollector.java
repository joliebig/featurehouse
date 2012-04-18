class AspectJCollector {
    public void PointcutDeclaration() throws ParseException {
        int begin = this.token.beginLine;
        super.PointcutDeclaration();
        int end = this.token.endLine;
        
        stats.pointcut_count++;
    }
}
