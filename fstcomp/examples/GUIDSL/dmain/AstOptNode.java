class AstOptNode {
    public void harvest( Visitor v ) {
        this.visit( v );
        if ( arg[0]!=null ) {
            arg[0].harvest( v );
        }
    }
    public void visit( Visitor v ) {
        v.action( this );
    }
}
