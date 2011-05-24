import Jakarta.util.*;
import java.util.*;

class not {
   public void reduce( cnfClause c ) {
      cterm t = new cterm(true);
      left.reduce(t);
      c.add(t);
   }

   public void reduce( ArrayList terms ) {
      cnfClause c = new cnfClause();
      left.reduce(c);
      terms.add(c);
   }
}
