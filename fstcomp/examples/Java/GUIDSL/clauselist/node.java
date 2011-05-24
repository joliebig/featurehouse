import Jakarta.util.*;
import java.util.*;

 abstract class node {
   void reduce( ArrayList terms ) {
      Util.fatalError( this.getClass().getName() + ".reduce(ArrayList) called");
   }

   void reduce( cnfClause c ) {
      Util.fatalError( this.getClass().getName() + ".reduce(cnfClause) called" );
   }

   void reduce( cterm t ) {
      Util.fatalError( this.getClass().getName() + ".reduce(cterm) called" );
   }
}
