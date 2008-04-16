import java.util.*;

public class variable implements Comparator {

   // for comparing & sorting variables

   public int compare(Object o1, Object o2) {
      variable v1 = (variable) o1;
      variable v2 = (variable) o2;

      if (v1.whenSet < v2.whenSet) return -1;
      if (v1.whenSet == v2.whenSet) return 0;
      return 1;
   }

   static int timer;
   int whenSet;           // variable assignment order
   node reason;           // formula that lead to the deduction
   ArrayList antecedents; // variables used in computing variable's value
   boolean   isRoot;      // is root variable?

   public void reset() {
      whenSet = 0;
      reason = null;
      antecedents = null;
      isRoot = false;
      original();
   }

   void resetRoot() {
      timer = 1;
      whenSet = 0;
      reason = null;
      antecedents = null;
      isRoot = true;
      original();
   }

   public void set( boolean negated ) {
      original(negated);
      whenSet = timer++;
   }

   // forms union of all antecedents

   void collectAntecedents( TreeSet ts ) {
      if (antecedents == null) return;
      Iterator i = antecedents.iterator();
      while (i.hasNext()) {
         variable v = (variable) i.next();
         if (!ts.contains(v)) {
           ts.add(v);
           v.collectAntecedents(ts);
         }
      }
   }

   String shortName() {
      if (name.startsWith("_")) return name.substring(1);
      return name;
   }

   String explainVariable() {
      String x;
  
      // this is a hack, due to the strange naming conventions.
      // we don't want to generate explanations of the form
      // MSTPrim because (_MSTPrim iff MSTPrim)
      // so we weed out any "reason" that generates (_x iff x)

      if (reason instanceof iff) 
         if (reason.left.toString().equals("_"+reason.right.toString()))
            return "";
 
      if (value == variable.U) return "";
      if (value == variable.F) x = "not " + shortName();
      else x = shortName();
      if (antecedents == null)
         if (isRoot)
            return x + " because it is root of grammar\n";
         else
            return x + " because set by user\n";
      return x + " because " + reason.toString().replace("_","") + "\n";
   }

   String explainValue() {   // returns explanation of why variable has its value
      TreeSet ts = new TreeSet((Comparator)this);

      // collect all variables together that contribute to this variable's
      // value

      ts.add((variable) this);
      collectAntecedents( ts );

      // now, iterate over this set in order from ground up

      String result = "";
      Iterator i = ts.iterator();
      while (i.hasNext()) {
         variable v = (variable) i.next();
         result = result + v.explainVariable();
      }
      return result;
   }


   // justify remembers why a variable's value was set the way it was

   public void justify( cnfClause r ) {
      reason = r.formula;
      antecedents = new ArrayList();

      Iterator i = r.terms.iterator();
      while (i.hasNext()) {
         cterm c = (cterm) i.next();
         if (!c.var.equals((variable) this))
             antecedents.add(c.var);
      }

      original(r);
   }

   public void justify() {
      reason = null;
      antecedents = null;
      isRoot = false;
      whenSet = timer++;   // root is always the first
      original();
   }
}
