class AspectJCollector {
    public void InterTypeDeclareBody() throws ParseException {
        int begin = this.token.beginLine;
        super.InterTypeDeclareBody();
        int end = this.token.endLine;
        
        stats.inter_type_declare_count++;
    }
}
