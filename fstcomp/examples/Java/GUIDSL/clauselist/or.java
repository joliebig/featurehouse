import Jakarta.util.*;
import java.util.*;

class or {
   public void reduce( ArrayList terms ) {
      cnfClause c = new cnfClause();
      reduce(c);
      terms.add(c);
   }
   
   public void reduce( cnfClause c ) {
     left.reduce(c);
     right.reduce(c);
   }
}
