public class star extends term {

    star( String name ) {
      super( name );
        pattern.current.terms.add( this );
    }
      
    public void visit( GVisitor v ) {
        v.action( this );
    }
}
