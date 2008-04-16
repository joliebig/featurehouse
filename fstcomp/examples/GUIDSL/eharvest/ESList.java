import java.util.*;

class ESList {

   public static ArrayList CTable = new ArrayList(); 
	// CTable contains all additional constraints

   public static void dumpCTable() {
        System.out.println( "-------Begin Ctable Dump----------" );
		  int cnt = CTable.size();
		  for (int i = 0; i<cnt; i++) {
            node n = ( node ) CTable.get(i);
            System.out.println(n);
        }
        System.out.println( cnt + " additional constraints in all." );
        System.out.println( "-------End Ctable Dump----------" );
    }

   // harvest all additional constraints ....

   node eharvest() {

      AstCursor c = new AstCursor();
      for (c.FirstElement(this); c.MoreElement(); c.NextElement() ) {
        ExprStmt es = (ExprStmt) c.node ;
		  es.eharvest();
		}
	   return null;
   }
}

