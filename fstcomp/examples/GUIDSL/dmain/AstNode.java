abstract class AstNode {
    public void harvest( Visitor v ) {
        int i;
        if ( arg == null )
            return;
        this.visit( v );
        for ( i=0; i<arg.length; i++ )
            if ( arg[i]!=null ) {
                arg[i].harvest( v );
            }
    }
    public void visit( Visitor v ) {
        v.action( this );
    }
}
