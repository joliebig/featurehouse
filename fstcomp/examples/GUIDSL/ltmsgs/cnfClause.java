import java.util.*;
import javax.swing.*;

class cnfClause
{
    private static void AnnounceProblem( String whatIsIt ) {
       JOptionPane.showMessageDialog(null,
             "The following clause is not satisfied:\n" + whatIsIt,
             "Program Specification Incomplete", JOptionPane.ERROR_MESSAGE);
     }


    static boolean complete()
    {
        int formulaIndex = 0;

        // populate list of clause strings if not done already
        if(grammar._originals.size() == 0)
        {   grammar.propagate();
            grammar.reset();
        }

        // go through production formulas
        Iterator i = (production.Ptable.values()).iterator();
        while(i.hasNext())
        {   production p = (production) (i.next());
            node temp = (p.formula).simplify().cnf();

            if(temp instanceof and)
            {   ArrayList formulas = new ArrayList();
                grammar.splitFormula(formulas, temp);
                for(int j = 0; j < formulas.size(); j++)
                {   String _original = (String) grammar._originals.get(formulaIndex);
                    clause c = new clause((node) formulas.get(j), null, _original);
                    analyzeClause(c, c.n);

                    // if clause is not satisfied, then spec is not complete
                    if(!grammar.SATISFIED.equals(c.value))
                    {   AnnounceProblem( c._original );
                        return false;
                    }
                }
            }
            else
            {   String _original = (String) grammar._originals.get(formulaIndex);
                clause c = new clause(temp, null, _original);
                analyzeClause(c, c.n);

                // if clause is not satisfied, then spec is not complete
                if(!grammar.SATISFIED.equals(c.value))
                {   AnnounceProblem( c._original );
                    return false;
                }
            }

            formulaIndex++;
        }

        // go through pattern formulas
        i = (pattern.Ttable.values()).iterator();
        while(i.hasNext())
        {   pattern p = (pattern) (i.next());
            node temp = (p.formula).simplify().cnf();

            if(temp instanceof and)
            {   ArrayList formulas = new ArrayList();
                grammar.splitFormula(formulas, temp);
                for(int j = 0; j < formulas.size(); j++)
                {   String _original = (String) grammar._originals.get(formulaIndex);
                    clause c = new clause((node) formulas.get(j), null, _original);
                    analyzeClause(c, c.n);

                    // if clause is not satisfied, then spec is not complete
                    if(!grammar.SATISFIED.equals(c.value))
                    {   AnnounceProblem( c._original );
                        return false;
                    }
                }
            }
            else
            {   String _original = (String) grammar._originals.get(formulaIndex);
                clause c = new clause(temp, null, _original);
                analyzeClause(c, c.n);

                // if clause is not satisfied, then spec is not complete
                if(!grammar.SATISFIED.equals(c.value))
                {   AnnounceProblem( c._original );
                    return false;
                }
            }

            formulaIndex++;
        }

        // go through additional constraints
        for(int k = 0; k < ESList.CTable.size(); k++)
        {   node p = (node) (ESList.CTable.get(k));
            node temp = p.simplify().cnf();

            if(temp instanceof and)
            {   ArrayList formulas = new ArrayList();
                grammar.splitFormula(formulas, temp);
                for(int j = 0; j < formulas.size(); j++)
                {   String _original = (String) grammar._originals.get(formulaIndex);
                    clause c = new clause((node) formulas.get(j), null, _original);
                    analyzeClause(c, c.n);

                    // if clause is not satisfied, then spec is not complete
                    if(!grammar.SATISFIED.equals(c.value))
                    {   AnnounceProblem( c._original );

                        return false;
                    }
                }
            }
            else
            {   String _original = (String) grammar._originals.get(formulaIndex);
                clause c = new clause(temp, null, _original);
                analyzeClause(c, c.n);

                // if clause is not satisfied, then spec is not complete
                if(!grammar.SATISFIED.equals(c.value))
                {   AnnounceProblem( c.__original );
                    return false;
                }
            }

            formulaIndex++;
        }

        // all clauses are satisfied
        return true;
    }


   static void analyzeClause(clause c, node n)
   {
        if(n instanceof bterm)
        {   variable v = (variable) (variable.Vtable.get(n.toString()));
            if(v.value == variable.T)
            {   c.value = grammar.SATISFIED;
                return;
            }
            else
            {   if(c.value == null)
                {   c.value = grammar.VIOLATED;
                }
            }
        }
        else if(n instanceof not)
        {   if(n.left != null)
            {   variable v = (variable) (variable.Vtable.get((n.left).toString()));
                if(v.value == variable.T)
                {   if(c.value == null)
                    {   c.value = grammar.VIOLATED;
                    }
                }
                else
                {   c.value = grammar.SATISFIED;
                    return;
                }
            }
        }
        else    // n instanceof or
        {   if(n.left != null)
            {   analyzeClause(c, n.left);
            }
            if(n.right != null)
            {   analyzeClause(c, n.right);
            }
        }
   }
}

