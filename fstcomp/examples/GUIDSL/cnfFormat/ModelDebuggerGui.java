// ModelDebuggerGui.java

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.filechooser.*;
import java.io.*;

public class ModelDebuggerGui extends  SwingDialog {
   public static ModelDebuggerGui itsme = null;
    JFrame owner = null;

   // initialize constants used in the application
   // REMEMBER -- make constants static!

   public void initConstants() {

   }

   // declare and initialize atomic components here

    JButton Open;
    JButton Save;
   JButton Clear;
    JCheckBox CnfFile;
    JTextArea Area;
    JScrollPane jsp;
   JFileChooser chooser;

   public void initAtoms() {
       Open = new JButton( "Open" );
        Open.setToolTipText("open model debugging file");
        Open.setBorder( BorderFactory.createRaisedBevelBorder() );
        Save = new JButton( "Save" );
        Save.setToolTipText("save debugging output");
        Save.setBorder( BorderFactory.createRaisedBevelBorder() );
        Clear = new JButton( "Clear" );
        Clear.setToolTipText( "clear screen" );
        Clear.setBorder( BorderFactory.createRaisedBevelBorder() );
        CnfFile = new JCheckBox( "CNF File" );
        CnfFile.setToolTipText( "save the CNF file" );
        CnfFile.setSelected( false );
        Area = new JTextArea( 20, 30 );
        Area.setEditable(false);
        jsp = new JScrollPane( Area );
        chooser = new JFileChooser(".");
        chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
   }

   // declare and initialize layout components here

    JPanel Buttons;
   public void initLayout() {
        Buttons = new JPanel();
        Buttons.setLayout( new GridLayout(0,1) );
        //Buttons.setLayout( new BoxLayout(Buttons, BoxLayout.Y_AXIS) );
        Buttons.add(Open);
        Buttons.add(Save);
        Buttons.add(Clear);
        Buttons.add(CnfFile);
   }

   // initialize ContentPane here

   public void initContentPane() {
      ContentPane = new JPanel();
      ContentPane.setLayout( new FlowLayout(FlowLayout.LEFT) );
      ContentPane.setBorder(BorderFactory.createEtchedBorder());
      ContentPane.add(Buttons);
        ContentPane.add(jsp);
   }

   // initialize listeners here

   public void initListeners() {
      Clear.addActionListener( new ActionListener() {
         public void actionPerformed( ActionEvent e ) {
               Area.setText("");
            }
        });

      Open.addActionListener(new ActionListener() {
         public void actionPerformed(ActionEvent e) {
             int returnVal = chooser.showOpenDialog(owner);

             if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = chooser.getSelectedFile();
                     String name = file.getName();
                try {
                        Area.setText("");
                        //itsme = ModelDebuggerGui.this;
                   solverTest.modelDebug( name, CnfFile.isSelected() );
                }
                catch ( Exception ee ) {
                   Area.append( "Exception in processing " + name + " " +
                              ee.getMessage() + "\n");
                   Area.append( "Processing of " + name + " aborted" );
                     }
             } else {
                Area.append("Open command cancelled by user./n");
             }
            }
        });

      Save.addActionListener(new ActionListener() {
         public void actionPerformed(ActionEvent e) {
             int returnVal = chooser.showOpenDialog(owner);

             if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = chooser.getSelectedFile();
                     try {
                        PrintWriter pw = new PrintWriter( new FileOutputStream(file) );
                        pw.print(Area.getText());
                        pw.close();
                     }
                catch ( Exception ee ) {
                        String er = "Exception in writing file " + ee.getMessage();
                    JOptionPane.showMessageDialog(null, er,
             "Error!", JOptionPane.ERROR_MESSAGE);
                     }
             }
                 Area.setText("");
            }
        });
   }

   // place in this method any action for exiting application

   public void applicationExit() {

   }

    void print( String x ) { Area.append(x); }

    void println( String x ) { Area.append(x + "\n");  }

   public ModelDebuggerGui(JFrame owner, boolean modal) {
      super(owner, modal);
      setLocationRelativeTo(owner);
       itsme = (ModelDebuggerGui) this;
        this.owner = owner;
   }

   public ModelDebuggerGui(JFrame owner, String AppTitle, boolean modal) {
      super(owner, AppTitle, modal);
      setLocationRelativeTo(owner);
        itsme = (ModelDebuggerGui) this;
        this.owner = owner;
   }

}
