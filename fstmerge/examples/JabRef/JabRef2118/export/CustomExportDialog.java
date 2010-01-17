
package net.sf.jabref.export; 

import java.awt.*; 
import java.awt.event.*; 

import javax.swing.*; 
import java.util.Vector; 
import java.io.File; 
import net.sf.jabref.JabRefFrame; 
import net.sf.jabref.Util; 

import net.sf.jabref.Globals; 

import com.jgoodies.forms.builder.ButtonBarBuilder; 
import com.jgoodies.forms.layout.Sizes; 

import java.awt.BorderLayout; 
import java.awt.GridBagConstraints; 
import java.awt.GridBagLayout; 
import java.awt.Insets; 
import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 


 


class  CustomExportDialog  extends JDialog {
	

    JTextField
    name = new JTextField(60),
    layoutFile = new JTextField(60),
    extension = new JTextField(60);

	
    JLabel
    nl = new JLabel(Globals.lang("Export name")+":"),
    nr = new JLabel(Globals.lang("Main layout file")+":"),
    nf = new JLabel(Globals.lang("File extension")+":");

	
    JButton
        ok = new JButton(Globals.lang("Ok")),
        cancel = new JButton(Globals.lang("Cancel")),
        browse = new JButton(Globals.lang("Browse"));

	
    JPanel
    main = new JPanel(),
    buttons = new JPanel();

	
    private boolean ok_pressed = false;

	
    

	
    private int index;

	
    private JabRefFrame parent;

	

    private String  oldName, oldRegexp, oldField;

	

    GridBagLayout gbl = new GridBagLayout();

	
    GridBagConstraints con = new GridBagConstraints();

	


    public CustomExportDialog(JabRefFrame parent_, String name_, String layoutFile_, String extension_) {
      this(parent_);
      name.setText(name_);
      layoutFile.setText(layoutFile_);
      extension.setText(extension_);
    }


	


    public CustomExportDialog(JabRefFrame parent_) {
    super(parent_, Globals.lang("Edit custom export"), true);
    parent = parent_;
    
    
    
    ActionListener okListener = new ActionListener() {
        public void actionPerformed(ActionEvent e) {

            
            if ((layoutFile.getText().equals("")) ||
            (name.getText().equals("")) ||
            (extension.getText().equals("")) ||
                        (!layoutFile.getText().endsWith(".layout"))) {
            
            
            
            
            
            return;
            }

            

            ok_pressed = true;
            dispose();
        }
        };
    ok.addActionListener(okListener);
    name.addActionListener(okListener);
    layoutFile.addActionListener(okListener);
    extension.addActionListener(okListener);

    AbstractAction cancelAction = new AbstractAction() {
          public void actionPerformed(ActionEvent e) {
              dispose();
        }
        };

    cancel.addActionListener(cancelAction);

        browse.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            File directory = new File(Globals.prefs.get("exportWorkingDirectory"));
            String chosenStr = Globals.getNewFile(parent, directory, ".layout",
                                             JFileChooser.OPEN_DIALOG, false);
            if (chosenStr == null) return;
            File chosen = new File(chosenStr);

            
            Globals.prefs.put("exportWorkingDirectory", chosen.getParent());

            layoutFile.setText(chosen.getPath());
          }
        });

        
        ActionMap am = main.getActionMap();
        InputMap im = main.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(parent.prefs().getKey("Close dialog"), "close");
    am.put("close", cancelAction);


    
    main.setLayout(gbl);
    main.setBorder(BorderFactory.createTitledBorder
               (BorderFactory.createEtchedBorder(),
            Globals.lang("Export properties")));

    
    con.weightx = 0;
    con.gridwidth = 1;
    con.insets = new Insets(3, 5, 3, 5);
    con.anchor = GridBagConstraints.EAST;
    con.fill = GridBagConstraints.NONE;
    con.gridx = 0;
    con.gridy = 0;
    gbl.setConstraints(nl, con);
    main.add(nl);
    con.gridy = 1;
    gbl.setConstraints(nr, con);
    main.add(nr);
    con.gridy = 2;
    gbl.setConstraints(nf, con);
    main.add(nf);

        con.gridwidth = 2;
        con.weightx = 1;
    con.anchor = GridBagConstraints.WEST;
    con.fill = GridBagConstraints.HORIZONTAL;
    con.gridy = 0;
    con.gridx = 1;
    gbl.setConstraints(name, con);
    main.add(name);
    con.gridy = 1;
        con.gridwidth = 1;
        gbl.setConstraints(layoutFile, con);
    main.add(layoutFile);
        con.gridx = 2;
        con.weightx = 0;
        gbl.setConstraints(browse, con);
        main.add(browse);
        con.weightx = 1;
        con.gridwidth = 2;
        con.gridx = 1;
    con.gridy = 2;
    gbl.setConstraints(extension, con);
    main.add(extension);

    ButtonBarBuilder bb = new ButtonBarBuilder(buttons);
    buttons.setBorder(BorderFactory.createEmptyBorder(2,2,2,2));
    bb.addGlue();
    bb.addGridded(ok);
    bb.addGridded(cancel);
    bb.addGlue();
    
    getContentPane().add(main, BorderLayout.CENTER);
    getContentPane().add(buttons, BorderLayout.SOUTH);

    
    setSize(600, 170);

        Util.placeDialog(this, parent);
    }


	

    public boolean okPressed() {
    return ok_pressed;
    }


	

    public int index() { return index; }


	
    public String oldField() { return oldField; }


	
    public String oldName() { return oldName; }


	
    public String oldRegexp() { return oldRegexp; }


	
    public String layoutFile() { return layoutFile.getText(); }


	
    public String name() { return name.getText(); }


	
    public String extension() {
      String ext = extension.getText();
      if (ext.startsWith("."))
        return ext;
      else if (ext.startsWith("*."))
        return ext.substring(1);
      else
        return "."+ext;
    }



}
