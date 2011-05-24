// this is a visitor that encapsulates actions per AstNode
// the action is to register the names of productions

import java.util.*;
import Jakarta.util.*;

class fillFPtable implements Visitor {

   public void action( AstNode n ) { }
   public void action( AstOptNode n ) { }
   public void action( AstList n ) { }
   public void action( AstListNode n ) { }

   public void action( AExpr n ) { }
   public void action( Avar n ) { }
   public void action( AvarList n ) { }
   public void action( BExpr n ) { }
   public void action( BAnd n ) { }
   public void action( BChoose1 n ) { }
   public void action( BIff n ) { }
   public void action( BImplies n ) { }
   public void action( BNot n ) { }
   public void action( BOr n ) { }
   public void action( Bvar n ) { }
   public void action( Cons n ) { }
   public void action( ConsStmt n ) { }

   public void action( EExpr n ) { }
   public void action( ESList n ) { }
   public void action( EStmt n ) { }
   public void action( Expr n ) { }
   public void action( ExprList n ) { }
   public void action( ExprStmt n ) { }
   public void action( GPattern n ) { }
   public void action( GProd n ) {}

   public void action( GProduction n ) {
      String name = n.tok[0].getTokenName();
      if (production.FPtable.containsKey(name)) {
         Util.error( "production " + name + " has multiple definitions");
         return;
      }
      production.FPtable.put(name,name);
   }

   public void action( GTerm n ) { }
   public void action( IExpr n ) { }
   public void action( MainModel n ) { }
   public void action( Model n ) { }
   public void action( NExpr n ) { }
   public void action( OExpr n ) { }
   public void action( Opt n ) { }
   public void action( OptTerm n ) { }
   public void action( Optid n ) { }
   public void action( Opts n ) { }
   public void action( Paren n ) { }
   public void action( Pat n ) { }
   public void action( Pats n ) { }
   public void action( PlusTerm n ) { }
   public void action( Prods n ) { }
   public void action( SimplePattern n ) { }
   public void action( StarTerm n ) { }
   public void action( Strlit n ) { }
   public void action( TermList n ) { }
   public void action( TermName n ) { }
   public void action( Var n ) { }
   public void action( Vars n ) { }
   public void action( VarStmt n ) { }
   public void action( enterGspec n ) { }
}
