interface GVisitor {
    void action( grammar n );
    void action( optprim n );
    void action( optprod n );
    void action( pattern n );
    void action( plus n );
    void action( prim n );
    void action( prod n );
    void action( production n );
    void action( star n );
    void action( term n );
    void action( variable n );
}
