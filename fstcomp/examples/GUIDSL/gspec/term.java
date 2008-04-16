import Jakarta.util.*;

public abstract class term extends gObj {
    static term current;
    production prod;

    term( String name ) {
      super( name );
        var = null;
        prod = null;
    }

    public void visit( GVisitor v ) {
        v.action( this );
    }

    public production findProduction( String name ) {
        prod = ( production ) production.Ptable.get( name );
        if ( prod == null ) {
            Util.error( name + " used in pattern, but not defined as production" );
            prod = new production(name);
            variable.define( name, variable.Prod, (gObj) null, false ); //do not redefine here
        }
        return prod;
    }
}
