import java.util.*;
import Jakarta.util.*;

public class order implements GVisitor {
   int Counter;

   public void outln(String s) { System.out.println(s); }
   public void out(String s) { System.out.print(s); }

   public void action( grammar n ) {
      // count number of primitive tokens in grammar
      Counter = 0;
      Iterator i = variable.Vtable.values().iterator();
      while ( i.hasNext() ) {
         variable v = ( variable ) i.next();
         if (v.type == variable.Prim)
            Counter++;
      }

      //outln("---------Variable Rankings---------------------------");
      n.traverse(this);
      //outln("---------End Var  Rankings---------------------------");
   }
   public void action( optprim n ) {
      n.var.rank = Counter--;
      //outln(n.var.rank + "\t " + n.name);
   }
   public void action( optprod n ) {
      (n.prod).traverse(this);
   }
   public void action( pattern n ) {
      n.traverse(this);
   }
   public void action( plus n ) {
      (n.prod).traverse(this);
   }
   public void action( prim n ) {
      n.var.rank = Counter--;
      //outln(n.var.rank + "\t " + n.name);
   }
   public void action( prod n ) {
      (n.prod).traverse(this);
   }
   public void action( star n ) {
      (n.prod).traverse(this);
   }
   public void action( production n ) {
      n.traverse(this);
   }
   public void action( term n ) {
      Util.fatalError("should never call ordergs.action(term)");
   }
   public void action( variable n ) {
      Util.fatalError("should never call ordergs.action(variable)");
   }
}
