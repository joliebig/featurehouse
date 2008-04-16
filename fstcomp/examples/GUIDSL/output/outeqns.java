import Jakarta.util.*;
import java.util.*;
import java.io.*;

class outeqns implements GVisitor {

   boolean debug = false;    // turn on for debugging

   PrintStream ps = null;
   Stack stack;
   LinkedList eqn;

   // output queued layer names -- we have to go to these hoops
   // so that the base term goes first (sigh)

   void outputEqn() {
      int z = eqn.size();
      for (int i = 0; i<z; i++) {
        String x = (String) eqn.removeLast();
	ps.print(x + " " );
      }
      ps.println();
      eqn = new LinkedList();
   }

   outeqns( File fl ) {
       try {
         ps =  (debug) ?  System.out :
                  new PrintStream( new FileOutputStream( fl, true/*append*/));
                  ps.println();
           stack = new Stack();
           eqn   = new LinkedList();
        }
        catch (Exception e) {
           Util.fatalError( "File not found");
        }


    }

   public void action( grammar n ) {
      if (!Main.equationFormat) ps.print("this = ");
      n.traverse(this);
      outputEqn();

        while (!stack.empty()) {
           production p = (production) stack.pop();
            if (!Main.equationFormat) ps.print(p.name + " = ");
            p.traverse(this);
            outputEqn();
        }
        ps.close();
   }

   public void action( optprim n ) {
       if (n.var.value == variable.T){
           eqn.add(n.var.out);
       }
    }

   public void action( optprod n ) {
       if (n.var.value == variable.T) {
         production p = production.find(n.name);
         action(p);
       }
   }

   public void action( pattern n ) {
       if (n.var.value == variable.T)
         n.traverse(this);
    }

   public void action( plus n ) {
       if (n.var.value == variable.T) {
         production p = production.find(n.name);
         action(p);
       }
   }

   public void action( prim n ) {
       if (n.var.value == variable.T){
           eqn.add(n.var.out);
       }
    }

   public void action( prod n ) {
       if (n.var.value == variable.T) {
          production p = production.find(n.name);
          action(p);
       }
   }

   public void action( star n ) {
       if (n.var.value == variable.T) {
          production p = production.find(n.name);
          action(p);
       }
   }

   public void action( production n ) {
       if (n.var.eqn) {
           eqn.add(n.name);
           stack.push(n);
           return;
        }
       if (n.var.value == variable.T) {
        production p = production.find(n.name);
        p.traverse(this);
        }
    }

   public void action( term n ) {
      System.err.println( "outeqns.term should not be called" );
    }

   public void action( variable n ) {
      System.err.println( "outeqns.variable should not be called" );
   }
}
