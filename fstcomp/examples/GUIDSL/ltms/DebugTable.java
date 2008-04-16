import java.util.*;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JComponent;
import javax.swing.BoxLayout;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableColumn;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Insets;

public class DebugTable extends JPanel {
    private static int ITEMS_PER_ROW = 3;
    private boolean ALLOW_COLUMN_SELECTION = false;
    private boolean ALLOW_ROW_SELECTION = true;

    static String[] columnNames = null;
    static String[][] data = null;

    static String[] sortedVtable = null;

    static DebugTable contentPane;
    static JFrame frame;

    public DebugTable()
    {
        super( new GridLayout( 0, 1 ) );

        columnNames = new String[ITEMS_PER_ROW];
        columnNames[0] = "Variable";
        columnNames[1] = "Value";
        columnNames[2] = "User Set";

        data = new String[variable.Vtable.size()][ITEMS_PER_ROW];

        sortedVtable = new String[variable.Vtable.size()];
        Iterator vars = ( variable.Vtable.values() ).iterator();
        for( int i = 0; i < sortedVtable.length; i++ )
        {
            variable v = ( variable ) ( vars.next() );
            sortedVtable[i] = v.name;
        }
        Arrays.sort( sortedVtable );

        int row = 0;
        for( int i = 0; i < sortedVtable.length; i++ )
        {
            variable v = ( variable ) ( variable.Vtable.get( sortedVtable[i] ) );
            data[row][0] = v.name;

            if( v.value == variable.T )
            {
                DebugTable.data[row][1] = "True";
            }
            else
                if( v.value == variable.F )
            {
                    DebugTable.data[row][1] = "False";
                }
                else    // v.value == variable.U
            {
                    DebugTable.data[row][1] = "Unknown";
                }

            if( v.userSet )
            {
                data[row][2] = "True";
            }
            else
            {
                data[row][2] = "False";
            }

            row++;
        }

        final JTable table = new JTable( data, columnNames );

        // Adjust widths of Value and User Set columns
        TableColumn column = table.getColumnModel().getColumn( 1 );
        column.setPreferredWidth( 30 );
        column = table.getColumnModel().getColumn( 2 );
        column.setPreferredWidth( 30 );

        table.setPreferredScrollableViewportSize( new Dimension( 500, 250 ) );

        table.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
        if ( ALLOW_ROW_SELECTION )        // true by default
        {
            ListSelectionModel rowSM = table.getSelectionModel();
            rowSM.addListSelectionListener( new ListSelectionListener() {
                public void valueChanged( ListSelectionEvent e )
                {}
            } );
        }
        else
        {
            table.setRowSelectionAllowed( false );
        }

        if ( ALLOW_COLUMN_SELECTION )     // false by default
        {
            table.setColumnSelectionAllowed( true );
            ListSelectionModel colSM = table.getColumnModel().getSelectionModel();
            colSM.addListSelectionListener( new ListSelectionListener() {
                public void valueChanged( ListSelectionEvent e )
                {}
            } );
        }

        // Display explanation when user clicks a row
        final JTextArea expl = new JTextArea();
        expl.setRows( 3 );
        expl.setMargin( new Insets( 5, 5, 5, 5 ) );
        expl.setEditable( false );
        expl.setLineWrap( true );
        expl.setWrapStyleWord( true );
        ListSelectionModel rowSM = table.getSelectionModel();
        rowSM.addListSelectionListener( new ListSelectionListener() {
            public void valueChanged( ListSelectionEvent e )
            {
                ListSelectionModel lsm = ( ListSelectionModel ) e.getSource();
                if( !lsm.isSelectionEmpty() )
                {
                    int r = lsm.getMinSelectionIndex();
                    variable v = ( variable ) ( variable.Vtable.get( sortedVtable[r] ) );
                    expl.setText( v.explainValue() );
                }
            }
        } );

        // Create the scroll panes and add the table and explanation to it
        JScrollPane scrollPane = new JScrollPane( table,
                                                 JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_NEVER );
        JScrollPane scrollPane2 = new JScrollPane( expl,
                                                  JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                  JScrollPane.HORIZONTAL_SCROLLBAR_NEVER );

        // Add the scroll panes to this panel
        add( scrollPane );
        add( scrollPane2 );
    }

    public static void update()
    {
        contentPane.updateUI();
    }

    public static void createAndShowGUI()
    {
        // Create and set up the window
        frame = new JFrame( "Variable Table" );
        frame.setDefaultCloseOperation( JFrame.DISPOSE_ON_CLOSE );

        // Create and set up the content pane
        contentPane = new DebugTable();
        contentPane.setLayout( new BoxLayout( contentPane, BoxLayout.Y_AXIS ) );
        contentPane.setOpaque( true ); // content panes must be opaque
        frame.setContentPane( contentPane );

        // Display the window
        frame.pack();
        frame.setVisible( true );
    }
}
