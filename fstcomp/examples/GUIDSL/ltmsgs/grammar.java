import java.util.*;
import javax.swing.JComponent;


class grammar
{
   public static final String UNKNOWN =       "Unknown";
   public static final String SATISFIED =     "Satisfied";
   public static final String VIOLATED =      "Violated";
   public static final String UNIT_OPEN =     "Unit-open";
   public static final String NON_UNIT_OPEN = "Non-unit-open";

   // this is the set of selections that are to be propagated by
   //  an LTMS
   static ArrayList UserSelections;

   static ArrayList violated;
   static ArrayList _originals;

   private static boolean debugIsOn = false;
   private static boolean updateNow = true;
   private static boolean beginning = true;
   static boolean store_originals = true;


   grammar(String name)
   {
        UserSelections = new ArrayList();
        violated = new ArrayList();
        _originals = new ArrayList();
   }


   static void initDebugTable()    // initialize debug table
   {
        if(beginning)
        {   reset();
        }

        DebugTable.createAndShowGUI();
        debugIsOn = true;
   }


   static void initFormulas()      // initialize propagation formulas table
   {
        Formulas.createAndShowGUI();
   }


   static void propagate()      // clear variables, and propagate UserSelections
   {
        beginning = false;

        updateNow = false;
        reset();
        updateNow = true;

        for(int i = 0; i < UserSelections.size(); i++)
        {   variable v = (variable) (UserSelections.get(i));
            v.value = variable.T;
            v.explanation = "Set by user";
        }

        satisfyClauses();

        if(debugIsOn)
        {   // update debug table components
            for(int row = 0; row < DebugTable.sortedVtable.length; row++)
            {   variable v = (variable)(variable.Vtable.get(DebugTable.sortedVtable[row]));
                if(v.value == variable.T)
                {   DebugTable.data[row][1] = "True";
                }
                else if(v.value == variable.F)
                {   DebugTable.data[row][1] = "False";
                }
                else    // v.value == variable.U
                {   DebugTable.data[row][1] = "Unknown";
                }

                if(v.userSet)
                {   DebugTable.data[row][2] = "True";
                }
                else
                {   DebugTable.data[row][2] = "False";
                }
            }
            DebugTable.update();
        }

        if(violated.size() > 0)
        {   JOptionPane.showMessageDialog(null,
                                          "Violations occured",
                                          "Invalid specification",
                                          JOptionPane.ERROR_MESSAGE);
        }
        violated.clear();

        store_originals = false;
   }


   public static void reset()     // reset variables
   {
        Iterator vars = (variable.Vtable.values()).iterator();
        while(vars.hasNext())
        {   variable v = (variable)(vars.next());
            v.value = variable.U;
            v.userSet = true;
            v.explanation = "";
        }
        String rootName = (grammar.rootProduction).name;
        variable root = (variable) (variable.Vtable.get(rootName));
        root.value = variable.T;
        root.userSet = false;
        root.explanation = "Root";

        if(debugIsOn && updateNow)
        {   // update debug table components
            for(int row = 0; row < DebugTable.sortedVtable.length; row++)
            {   variable v = (variable)(variable.Vtable.get(DebugTable.sortedVtable[row]));
                if(rootName.equals(v.name))
                {   DebugTable.data[row][1] = "True";
                    DebugTable.data[row][2] = "False";
                }
                else
                {   DebugTable.data[row][1] = "Unknown";
                    DebugTable.data[row][2] = "True";
                }
            }
            DebugTable.update();
        }
   }


   private static void satisfyClauses()
   {
        Stack clauses = new Stack();
        ArrayList nonUnit = new ArrayList();
        int formulaIndex = 0;

        // go through production formulas
        Iterator i = (production.Ptable.values()).iterator();
        while(i.hasNext())
        {   production p = (production) (i.next());
            if(store_originals)
            {   _originals.add(p.formula.toString());
            }
            node temp = (p.formula).simplify().cnf();

            if(temp instanceof and)
            {   ArrayList formulas = new ArrayList();
                splitFormula(formulas, temp);
                for(int j = 0; j < formulas.size(); j++)
                {   String _original = (String) _originals.get(formulaIndex);
                    clause c = new clause((node) formulas.get(j), null, _original);
                    checkClause(c, c.n);

                    // perform actions based on the value of c.
                    // if c.value == SATISFIED, then clause is already satisfied
                    if(VIOLATED.equals(c.value))
                    {   if(violated == null)
                        {   violated = new ArrayList();
                        }
                        violated.add(p.formula);
                    }
                    else if(UNKNOWN.equals(c.value))
                    {   if(c.unknowns == 1)
                        {   c.value = UNIT_OPEN;
                            clauses.push(c);
                        }
                        else    // c.unknowns > 1
                        {   c.value = NON_UNIT_OPEN;
                            nonUnit.add(c);
                        }
                    }
                }
            }
            else
            {   String _original = (String) _originals.get(formulaIndex);
                clause c = new clause(temp, null, _original);
                checkClause(c, c.n);

                // perform actions based on the value of c.
                // if c.value == SATISFIED, then clause is already satisfied
                if(VIOLATED.equals(c.value))
                {   if(violated == null)
                    {   violated = new ArrayList();
                    }
                    violated.add(p.formula);
                }
                else if(UNKNOWN.equals(c.value))
                {   if(c.unknowns == 1)
                    {   c.value = UNIT_OPEN;
                        clauses.push(c);
                    }
                    else    // c.unknowns > 1
                    {   c.value = NON_UNIT_OPEN;
                        nonUnit.add(c);
                    }
                }
            }

            formulaIndex++;
        }

        // go through pattern formulas
        i = (pattern.Ttable.values()).iterator();
        while(i.hasNext())
        {   pattern p = (pattern) (i.next());

            if(store_originals)
            {   _originals.add(p.formula.toString());
            }

            node temp = (p.formula).simplify().cnf();

            if(temp instanceof and)
            {   ArrayList formulas = new ArrayList();
                splitFormula(formulas, temp);
                for(int j = 0; j < formulas.size(); j++)
                {   String _original = (String) _originals.get(formulaIndex);
                    clause c = new clause((node) formulas.get(j), null, _original);
                    checkClause(c, c.n);

                    // perform actions based on the value of c.
                    // if c.value == SATISFIED, then clause is already satisfied
                    if(VIOLATED.equals(c.value))
                    {   violated.add(c.n);
                    }
                    else if(UNKNOWN.equals(c.value))
                    {   if(c.unknowns == 1)
                        {   c.value = UNIT_OPEN;
                            clauses.push(c);
                        }
                        else    // c.unknowns > 1
                        {   c.value = NON_UNIT_OPEN;
                            nonUnit.add(c);
                        }
                    }
                }
            }
            else
            {   String _original = (String) _originals.get(formulaIndex);
                clause c = new clause(temp, null, _original);
                checkClause(c, c.n);

                // perform actions based on the value of c.
                // if c.value == SATISFIED, then clause is already satisfied
                if(VIOLATED.equals(c.value))
                {   violated.add(c.n);
                }
                else if(UNKNOWN.equals(c.value))
                {   if(c.unknowns == 1)
                    {   c.value = UNIT_OPEN;
                        clauses.push(c);
                    }
                    else    // c.unknowns > 1
                    {   c.value = NON_UNIT_OPEN;
                        nonUnit.add(c);
                    }
                }
            }

            formulaIndex++;
        }

        // go through additional constraints
        for(int k = 0; k < ESList.CTable.size(); k++)
        {   node p = (node) (ESList.CTable.get(k));

            if(store_originals)
            {   _originals.add(p.toString());
            }

            node temp = p.simplify().cnf();

            if(temp instanceof and)
            {   ArrayList formulas = new ArrayList();
                splitFormula(formulas, temp);
                for(int j = 0; j < formulas.size(); j++)
                {   String _original = (String) _originals.get(formulaIndex);
                    clause c = new clause((node) formulas.get(j), null, _original);
                    checkClause(c, c.n);

                    // perform actions based on the value of c.
                    // if c.value == SATISFIED, then clause is already satisfied
                    if(VIOLATED.equals(c.value))
                    {   violated.add(c.n);
                    }
                    else if(UNKNOWN.equals(c.value))
                    {   if(c.unknowns == 1)
                        {   c.value = UNIT_OPEN;
                            clauses.push(c);
                        }
                        else    // c.unknowns > 1
                        {   c.value = NON_UNIT_OPEN;
                            nonUnit.add(c);
                        }
                    }
                }
            }
            else
            {   String _original = (String) _originals.get(formulaIndex);
                clause c = new clause(temp, null, _original);
                checkClause(c, c.n);

                // perform actions based on the value of c.
                // if c.value == SATISFIED, then clause is already satisfied
                if(VIOLATED.equals(c.value))
                {   violated.add(c.n);
                }
                else if(UNKNOWN.equals(c.value))
                {   if(c.unknowns == 1)
                    {   c.value = UNIT_OPEN;
                        clauses.push(c);
                    }
                    else    // c.unknowns > 1
                    {   c.value = NON_UNIT_OPEN;
                        nonUnit.add(c);
                    }
                }
            }

            formulaIndex++;
        }

        // propagate constraints
        while(true)
        {   boolean changed = false;
            while(!clauses.empty())
            {   clause c = (clause) clauses.pop();
                c.value = null;
                c.unknowns = 0;
                checkClause(c, c.n);

                // if c.value == SATISFIED, then nothing else needs to be done
                // on the clause
                if(UNKNOWN.equals(c.value))
                {   setTerm(c, c.n);
                    c.value = null;
                    c.unknowns = 0;
                    checkClause(c, c.n);
                    if(VIOLATED.equals(c.value))
                    {   violated.add(c.n);
                    }
                }
                else if(VIOLATED.equals(c.value))
                {   violated.add(c.n);
                }
            }
            for(int j = 0; j < nonUnit.size(); j++)
            {   clause c = (clause) nonUnit.get(j);
                c.value = null;
                c.unknowns = 0;
                checkClause(c, c.n);
                if(c.unknowns == 1)
                {   c.value = UNIT_OPEN;
                    nonUnit.remove(j);
                    j--;
                    clauses.push(c);
                    changed = true;
                }
                else    // c.unknowns > 1
                {   c.value = NON_UNIT_OPEN;
                }
            }
            if(!changed)
            {   break;
            }
        }
   }


   static void splitFormula(ArrayList formulas, node n)
   {
        if(n.left != null)
        {   if(n.left instanceof and)
            {   splitFormula(formulas, n.left);
            }
            else
            {   formulas.add(n.left);
            }
        }
        if(n.right != null)
        {   if(n.right instanceof and)
            {   splitFormula(formulas, n.right);
            }
            else
            {   formulas.add(n.right);
            }
        }
   }


   static void checkClause(clause c, node n)
   {
        if(n instanceof bterm)
        {   variable v = (variable) (variable.Vtable.get(n.toString()));
            if(v.value == variable.T)
            {   c.value = SATISFIED;
            }
            else if(v.value == variable.F)
            {   if(c.value == null)
                {   c.value = VIOLATED;
                }
            }
            else    // v.value == variable.U
            {   if(!SATISFIED.equals(c.value))
                {   c.value = UNKNOWN;
                    (c.unknowns)++;
                }
            }
        }
        else if(n instanceof not)
        {   if(n.left != null)
            {   variable v = (variable) (variable.Vtable.get((n.left).toString()));
                if(v.value == variable.T)
                {   if(c.value == null)
                    {   c.value = VIOLATED;
                    }
                }
                else if(v.value == variable.F)
                {   c.value = SATISFIED;
                }
                else    // v.value == variable.U
                {   if(!SATISFIED.equals(c.value))
                    {   c.value = UNKNOWN;
                        (c.unknowns)++;
                    }
                }
            }
        }
        else    // n instanceof or
        {   if(n.left != null)
            {   checkClause(c, n.left);
            }
            if(n.right != null)
            {   checkClause(c, n.right);
            }
        }
   }


   private static void setTerm(clause c, node n)
   {
        if(n instanceof bterm)
        {   variable v = (variable) (variable.Vtable.get(n.toString()));
            if(v.value == variable.U)
            {   v.value = variable.T;
                v.userSet = false;
                StringBuffer expl = new StringBuffer("");
                int edit = expl.length();
                buildExplanation(v.name, c.n, expl);
                String temp = expl.toString();
                if(temp.length() == edit)   // no assumptions
                {   v.explanation = c._original.toString();
                }
                else    // take out last comma
                {   temp = temp.substring(0, temp.length() - 2);
                    v.explanation = temp + " and " + c._original.toString();
                }
            }
        }
        else if(n instanceof not)
        {   if(n.left != null)
            {   variable v = (variable) (variable.Vtable.get((n.left).toString()));
                if(v.value == variable.U)
                {   v.value = variable.F;
                    v.userSet = false;
                    StringBuffer expl = new StringBuffer("");
                    int edit = expl.length();
                    buildExplanation(v.name, c.n, expl);
                    String temp = expl.toString();
                    if(temp.length() == edit)   // no assumptions
                    {   v.explanation = c._original.toString();
                    }
                    else    // take out last comma
                    {   temp = temp.substring(0, temp.length() - 2);
                        v.explanation = temp + " and " + c.__original.toString();
                    }
                }
            }
        }
        else    // n instanceof or
        {   if(n.left != null)
            {   setTerm(c, n.left);
            }
            if(n.right != null)
            {   setTerm(c, n.right);
            }
        }
   }


   private static void buildExplanation(String name, node n, StringBuffer expl)
   {
        if(n instanceof bterm)
        {   variable v = (variable) (variable.Vtable.get(n.toString()));

            // no need to include the variable in its own explanation
            if(name.equals(v.name))
            {   return;
            }

            if(v.value == variable.T)
            {   expl.append(v.name + "=true, ");
            }
            else if(v.value == variable.F)
            {   expl.append(v.name + "=false, ");
            }
            else    // v.value == variable.U
            {   expl.append(v.name + "=unknown, ");
            }
        }
        else if(n instanceof not)
        {   if(n.left != null)
            {   variable v = (variable) (variable.Vtable.get((n.left).toString()));

                // no need to include the variable in its own explanation
                if(name.equals(v.name))
                {   return;
                }

                if(v.value == variable.T)
                {   expl.append(v.name + "=true, ");
                }
                else if(v.value == variable.F)
                {   expl.append(v.name + "=false, ");
                }
                else    // v.value == variable.U
                {   expl.append(v.name + "=unknown, ");
                }
            }
        }
        else    // n instanceof or
        {   if(n.left != null)
            {   buildExplanation(name, n.left, expl);
            }
            if(n.right != null)
            {   buildExplanation(name, n.right, expl);
            }
        }
   }
}
