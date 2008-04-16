import java.util.*;

class BChoose1 {

   public node eharvest () {
	    node result = null;
		 ExprList el = (ExprList) arg[0];

       AstCursor c = new AstCursor();
		 for (c.FirstElement(el); c.MoreElement(); c.NextElement() ) {
          EExpr e = (EExpr) c.node ;
			 node n = e.eharvest();
			 if (result == null)
			    result = n;
			 else
			    result = new onlyone( n, result );
		}

		return result;
   }
}
