import java.util.*;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JComponent;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.Dimension;
import java.awt.GridLayout;

public class Formulas extends JPanel
{
    private boolean ALLOW_COLUMN_SELECTION = false;
    private boolean ALLOW_ROW_SELECTION = true;

    static String[] columnName = null;
    static String[][] data = null;

    static Formulas contentPane;
    static JFrame frame;


    public Formulas()
    {
        super(new GridLayout(1,0));

        columnName = new String[1];
        columnName[0] = "Formula";

        int total = production.Ptable.size() +
                    pattern.Ttable.size() +
                    ESList.CTable.size();
        data = new String[total][1];

        int index = 0;
        if(grammar.storeOriginals)
        {   Iterator vars = (production.Ptable.values()).iterator();
            while(vars.hasNext())
            {   production p = (production) (vars.next());
                data[index][0] = p.formula.toString();
                index++;
            }
            vars = (pattern.Ttable.values()).iterator();
            while(vars.hasNext())
            {   pattern p = (pattern) (vars.next());
                data[index][0] = p.formula.toString();
                index++;
            }
            for(int k = 0; k < ESList.CTable.size(); k++)
            {   node n = (node) (ESList.CTable.get(k));
                data[index][0] = n.toString();
                index++;
            }
        }
        else
        {   for(; index < grammar.originals.size(); index++)
            {   data[index][0] = (String) grammar.originals.get(index);
            }
        }

        final JTable table = new JTable(data, columnName);
        table.setPreferredScrollableViewportSize(new Dimension(500, 250));

        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        if (ALLOW_ROW_SELECTION)        // true by default
        {   ListSelectionModel rowSM = table.getSelectionModel();
            rowSM.addListSelectionListener(new ListSelectionListener()
            {   public void valueChanged(ListSelectionEvent e)
                {
                }
            });
        }
        else
        {   table.setRowSelectionAllowed(false);
        }

        if (ALLOW_COLUMN_SELECTION)     // false by default
        {   table.setColumnSelectionAllowed(true);
            ListSelectionModel colSM = table.getColumnModel().getSelectionModel();
            colSM.addListSelectionListener(new ListSelectionListener()
            {   public void valueChanged(ListSelectionEvent e)
                {
                }
            });
        }

        // Create the scroll pane and add the table to it
        JScrollPane scrollPane = new JScrollPane(table,
                                                 JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        // Add the scroll pane to this panel
        add(scrollPane);
   }


    public static void createAndShowGUI()
    {
        // Create and set up the window
        frame = new JFrame("Propagation Formulas");
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        // Create and set up the content pane
        contentPane = new Formulas();
        contentPane.setOpaque(true);        // content panes must be opaque
        frame.setContentPane(contentPane);

        // Display the window
        frame.pack();
        frame.setVisible(true);
    }
}
