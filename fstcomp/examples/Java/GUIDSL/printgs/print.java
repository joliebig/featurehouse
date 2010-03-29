import java.util.*;
import Jakarta.util.*;

class print implements GVisitor {
   Stack s;

   public void outln(String s) { System.out.println(s); }
   public void out(String s) { System.out.print(s); }

   public void action( grammar n ) {
      s = new Stack();
      outln("------------------------------------");
      outln("grammar " + n.name);
      n.traverse(this);
      while (!s.empty()) {
        production p = (production) s.pop();
        action(p);
      }
      outln("------------------------------------");
   } 
   public void action( optprim n ) {
      out(" [" + n.name +"] ");
   }
   public void action( optprod n ) {
      out(" [" + n.name +"] ");
      s.push(n.prod);
   }
   public void action( pattern n ) {
      out("	");
      n.traverse(this);
      outln(":: " + n.name);
   }
   public void action( plus n ) {
      out(" " + n.name + "+ ");
      s.push(n.prod);
   }
   public void action( prim n ) {
      out(" " + n.name + " " );
   }
   public void action( prod n ) {
      out(" " + n.name + " " );
      s.push(n.prod);
   }
   public void action( star n ) {
      out("   " + n.name +"* ");
      s.push(n.prod);
   }
   public void action( production n ) {
      outln(n.name + " " + n.getType() + ": " );
      n.traverse(this);
   }
   public void action( term n ) {
      Util.fatalError("should never call printgs.action(term)");
   }
   public void action( variable n ) {
      Util.fatalError("should never call printgs.action(variable)");
   }
}
