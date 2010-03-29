// SwingDialog.java - Abstract program that defines the basic "shell"
// of a Swing Application. Specific Swing applications are subclasses
// of SwingDialog

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class SwingDialog extends JDialog {
   // initialize constants used in the application

   public void initConstants() {}

   // declare atomic components here
   // initialize them in initAtoms()

   public void initAtoms() {}

   // declare layout components here
   // initialize them initLayout()

   public void initLayout() {}

   // ContentPane is declared here
   // and is initialized in initContentPane()

   public JPanel ContentPane;

   public void initContentPane() {}

   // declare component listeners here

   public void initListeners() {}

   // initialize entire containment hierarchy

   public void init() {
      initConstants();                   // initialize constants
      initAtoms();                       // initialize atoms
      initLayout();                      // initialize layout
      initContentPane();                 // initialize content pane
      getContentPane().add(ContentPane); // set ContentPane of window
      initListeners();                   // initialize listeners
      addWindowListener(	         // standard code to kill window 
            new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                    applicationExit();
                    dispose();
                }
            });
      pack();                            // start window by packing
      setVisible(true);
   }

   public SwingDialog(JFrame owner, boolean modal) {
      super(owner,modal);
      init();
   }

   public SwingDialog( JFrame owner, String AppTitle, boolean modal ) {
      super( owner, AppTitle, modal );	 // set title
      init();                            // initialize hierarchy
   }

   // place in this method any action for exiting application

   public void applicationExit() {}
}
