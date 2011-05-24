import java.util.*;
import Jakarta.util.*;

public class findRank implements GVisitor {
   int rank;
   String primitive;

   public void setPrimitive(String primitive){
    this.primitive=primitive;
    this.rank=0;
   }
   public int getRank(){return rank;}

   public void outln(String s) { }
   public void out(String s) { }

   public void action( grammar n ) {
      n.traverse(this);
   }
   public void action( optprim n ) {
      if (n.name.equals(primitive))
        rank=n.var.rank;
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
      if (n.name.equals(primitive))
        rank=n.var.rank;
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
