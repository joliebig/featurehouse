class AstList {
    public void harvest( Visitor v ) {
        AstNode l;
        if ( arg[0]==null )
            return;
        this.visit( v );
        for ( l = arg[0]; l!=null; l = l.right ) {
            if ( l.arg[0] == null )
                continue;
            l.arg[0].harvest( v );
        }
    }
    public void visit( Visitor v ) {
        v.action( this );
    }
}
