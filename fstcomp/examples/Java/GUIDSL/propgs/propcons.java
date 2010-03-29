import Jakarta.util.*;

public class propcons implements GVisitor {
   public void action( grammar n ) {
      common(n.rootProduction);
      n.rootProduction.type= production.norm;
      n.rootProduction.var.setValue( variable.T );
      n.traverse(this);
   }

   public void action( optprim n ) { common(n); }

   public void action( optprod n ) {
      production p = n.findProduction(n.name);
      common( n );
      common( p );
      n.prod.type = production.opt;
      p.traverse(this);
   }

   public void action( pattern n ) {
    common(n); n.traverse(this); }

   public void action( plus n ) {
      production p = n.findProduction(n.name);
      common( n );
      common( p );
      n.prod.type = production.plus;
      p.traverse(this);
   }

   public void action( prim n ) { common(n); }

   public void action( prod n ) {
     production p = n.findProduction(n.name);
     common( n );
     common( p );
     n.prod.type = production.norm;
     p.traverse(this);
   }

   public void action( star n ) {
     production p = n.findProduction(n.name);
     common( n );
     common( p );
     n.prod.type = production.star;
     p.traverse(this);
   }

   public void action( production n ) { common(n); n.traverse(this); }

   public void action( term n ) { }

   public void action( variable n ) {
      System.err.println( "prodcons.variable should not be called" );
   }

   public void common( gObj n ) {

     // link variable reference to its definition
     n.var = (variable) variable.Vtable.get(n.name);
     if (n.var == null) {
       Util.error(n.name + " used in pattern, but not defined as variable");
      // n.var = variable.define(n.name, variable.Prod, n ); //should not define at this point
     }
     n.var.gobj = n;
   }
}
