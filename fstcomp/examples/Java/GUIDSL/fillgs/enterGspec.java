// this is the visitor that encapsulates actions per AstNode
// the action is to harvest the AST and enter its data into
// the "grammar spec" files  of layer gspec

import java.util.*;
import Jakarta.util.*;

public class enterGspec implements Visitor {

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

   public void action( GPattern n ) {
     String name = n.tok[1].getTokenName();
     variable.define( name, variable.Patt, new pattern( name ), false );
   }

   public void action( GProd n ) {}

   public void action( GProduction n ) {
      String name = n.tok[0].getTokenName();
      variable.define( name, variable.Prod, new production(name), false );
   }

   public void action( GTerm n ) { }
   public void action( IExpr n ) { }
   public void action( MainModel n ) {
      variable.vtsize = 0; /*reset counter*/ }
   public void action( Model n ) { }
   public void action( NExpr n ) { }
   public void action( OExpr n ) { }
   public void action( Opt n ) { }
   public void action( OptTerm n ) {
      String name = n.tok[1].getTokenName();
      if (!production.FPtable.containsKey(name)) {
         variable.define( name, variable.Prim, new optprim(name), false);
      }
      else
         variable.define( name, variable.Prod, new optprod( name ), true );
   }


    public String opt ="";            // string that defines option
    public boolean foundOpt = false;  // is this a legal option?

   public void action( Optid n ) {
       opt = n.tok[0].getTokenName();
        foundOpt = false;
        processOptid();
        if (!foundOpt)
       Util.error("Unrecognizable option: " + opt );
    }

    public void processOptid() {
      /* no labels in basic grammar */
   }

   public void action( Opts n ) { }

   public void action( Paren n ) { }
   public void action( Pat n ) { }
   public void action( Pats n ) { }

   public void action( PlusTerm n ) {
      String name = n.tok[0].getTokenName();
      if (!production.FPtable.containsKey(name)) {
         Util.error( "cannot use primitive layer " + name +
                     " in a plus pattern");
         return;
      }
      variable.define( name, variable.Prod, new plus( name ), true );
   }

   public void action( Prods n ) { }

   public void action( SimplePattern n ) {
      String name = n.tok[0].getTokenName();
      if (production.FPtable.containsKey(name))
         Util.error("Cannot have a production " + name + " as a simple pattern");
      String patName = "_"+name;
      variable.define( patName, variable.Patt, new pattern( patName), false );
      variable.define( name, variable.Prim, new prim( name ), false );
   }

   public void action( StarTerm n ) {
      String name = n.tok[0].getTokenName();
      if (!production.FPtable.containsKey(name)) {
         Util.error( "cannot use primitive layer " + name +
                     " in a star pattern");
         return;
      }
      variable.define(name, variable.Prod, new star( name ), true);
   }

    public String optVal;

   public void action( Strlit n ) {
       opt = n.tok[0].getTokenName();
        optVal = n.tok[2].getTokenName();
        foundOpt = false;
        processStrlit();
        if (!foundOpt)
       Util.error("Unrecognizable option: " + opt );
   }

    public void processStrlit() {
       /* no options in base grammar */
    }

   public void action( TermList n ) { }

   public void action( TermName n ) {
      String name = n.tok[0].getTokenName();
      if (!production.FPtable.containsKey(name))
         variable.define(name, variable.Prim, new prim( name ), false);
      else
         variable.define(name, variable.Prod, new prod( name ), true);
   }

   public variable currentVar;     // current variable whose option list
                // we're processing

   public void action( Var n ) {
      String name = n.tok[0].getTokenName();
      currentVar = (variable) variable.Vtable.get(name);
      if (currentVar==null) {
         Util.error("variable " + name + " never used");
            currentVar = new variable();  // some dummy holder for now
        }
   }

   public void action( Vars n ) { }
   public void action( VarStmt n ) { }
}
