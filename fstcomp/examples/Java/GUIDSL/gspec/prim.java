public class prim extends term {

    prim( String name ) {
      super( name );
        current = this;
        pattern.current.terms.add( this );
    }

    public void visit( GVisitor v ) {
        v.action( this );
    }
}
