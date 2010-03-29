import Jakarta.util.*;
import java.util.*;

class bterm  {
   public void reduce( cterm t ) {
      variable v = variable.find(name);
      if (v == null)
         Util.error( "variable " + name + " undeclared");
      t.setVar(v);
   }

   public void reduce( ArrayList terms ) {
      cnfClause c = new cnfClause();
      reduce(c);
      terms.add(c);
   }

   public void reduce( cnfClause c ) {
      cterm t = new cterm(false);
      reduce(t);
      c.add(t);
   }
}
