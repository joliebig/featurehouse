
package net.sf.jabref;

import java.util.*;
import java.io.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

class EntryCustomizationDialog extends JDialog implements ItemListener
{
    BibtexEntryType type;

    JScrollPane reqSP, optSP;
    JButton ok, cancel, helpButton, delete, importTypes, exportTypes;
    JPanel panel=new JPanel(),
	fieldPanel = new JPanel(),
	typePanel = new JPanel();
    int width=10;
    JLabel messageLabel=new JLabel("", SwingConstants.CENTER);

    JTextField name = new JTextField("", width);
    JTextArea req_ta=new JTextArea("",5,width),
	opt_ta=new JTextArea("",5,width);
    

    JComboBox types_cb = new JComboBox();

    HelpAction help;

    GridBagLayout gbl = new GridBagLayout();
    GridBagConstraints con = new GridBagConstraints();
    JPanel buttonPanel = new JPanel();

    JabRefFrame parent;
    EntryCustomizationDialog ths = this;

    public EntryCustomizationDialog(JabRefFrame parent)
    {
	
	
	
	
	super(parent,Globals.lang("Customize entry types"), false);
	this.parent = parent;
	help = new HelpAction(parent.helpDiag, GUIGlobals.customEntriesHelp,
			      "Help", GUIGlobals.getIconUrl("helpSmall"));
	setTypeSelection();
	
	initialize();
	makeButtons();

	reqSP = new JScrollPane(req_ta,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	optSP = new JScrollPane(opt_ta,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	
	
        JToolBar tlb = new JToolBar();
        tlb.setFloatable(false);
        tlb.add(help);
	
	
	panel.setLayout(gbl);
	typePanel.setLayout(gbl);
	fieldPanel.setLayout(gbl);
	
	fieldPanel.setBorder(BorderFactory.createEtchedBorder());
	typePanel.setBorder(BorderFactory.createEtchedBorder());

	JLabel lab = new JLabel(Globals.lang("Type")+": "),
	    lab2 = new JLabel(Globals.lang("Name")+": ");
	con.insets = new Insets(5, 5, 5, 5);
	gbl.setConstraints(lab, con);
	gbl.setConstraints(lab2, con);
	gbl.setConstraints(types_cb, con);

	con.weightx = 1;
	con.fill = GridBagConstraints.HORIZONTAL;
	gbl.setConstraints(name, con);
	con.fill = GridBagConstraints.NONE;
	con.gridwidth = GridBagConstraints.REMAINDER;
	con.weightx = 0;
	
	gbl.setConstraints(tlb, con);
	con.gridwidth = 1;
	typePanel.add(lab);
	typePanel.add(types_cb);
	typePanel.add(lab2);
	typePanel.add(name);
	
	typePanel.add(tlb);
	lab = new JLabel(Globals.lang("Required fields"));
	con.fill = GridBagConstraints.BOTH;
	con.weightx = 1;
	gbl.setConstraints(lab, con);
	con.weighty = 1;
	gbl.setConstraints(reqSP, con);
	fieldPanel.add(lab);
	con.gridwidth = GridBagConstraints.REMAINDER;
	lab = new JLabel(Globals.lang("Optional fields"));
	con.weighty = 0;
	gbl.setConstraints(lab, con);
	fieldPanel.add(lab);
	con.weighty = 1;
	gbl.setConstraints(optSP, con);

	fieldPanel.add(reqSP);
	fieldPanel.add(optSP);

	con.gridwidth = GridBagConstraints.REMAINDER;
	con.weighty = 0;
	gbl.setConstraints(typePanel, con);
	con.weighty = 1;
	gbl.setConstraints(fieldPanel, con);
	con.weighty = 0;
	gbl.setConstraints(messageLabel, con);
	panel.add(typePanel);
	panel.add(fieldPanel);
	panel.add(messageLabel);

        
        ActionMap am = panel.getActionMap();
        InputMap im = panel.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(Globals.prefs.getKey("Close dialog"), "close");
        am.put("close", new AbstractAction() {
          public void actionPerformed(ActionEvent e) {
            dispose();
          }
        });
        pack();
	name.requestFocus();
    }

    public EntryCustomizationDialog(JabRefFrame parent,
				    BibtexEntryType type_) {
	this(parent);
	type = type_;

    }

    void initialize(){

	getContentPane().setLayout(new BorderLayout());
	getContentPane().add( buttonPanel, BorderLayout.SOUTH);
	getContentPane().add( panel, BorderLayout.CENTER);

	messageLabel.setForeground(Color.black);
	messageLabel.setText(Globals.lang("Delimit fields with semicolon, ex.")
			     +": author;title;journal");

	types_cb.addItemListener(this);
    }

    void save()
    {
      String typeName = name.getText().trim();
      if (typeName.indexOf(" ") >= 0) {
        JOptionPane.showMessageDialog(ths, Globals.lang("The type name can not contain spaces."),
                                      Globals.lang("Illegal type name"), JOptionPane.ERROR_MESSAGE);
        return;
      }


	String
	    reqStr = req_ta.getText().replaceAll("\\s+","")
	    .replaceAll("\\n+","").trim(),
	    optStr = opt_ta.getText().replaceAll("\\s+","")
	    .replaceAll("\\n+","").trim();


	if(! typeName.equals("")) {
	    CustomEntryType typ = new CustomEntryType
		(Util.nCase(typeName), reqStr, optStr);
	    BibtexEntryType.ALL_TYPES.put(typeName.toLowerCase(), typ);
	    updateTypesForEntries(typ.getName());
	    setTypeSelection();
	    messageLabel.setText(Globals.lang("Stored definition for type")+
				 " '"+Util.nCase(typ.getName())
				 +"'.");
	}
	else{
	    messageLabel.setText(Globals.lang("You must fill in a name for the entry type."));
	}

    }

    private void setTypeSelection() {
	types_cb.removeAllItems();
	types_cb.addItem("<new>");
	Iterator i = BibtexEntryType.ALL_TYPES.keySet().iterator();
	BibtexEntryType type;
	String toSet;
	while (i.hasNext()) {
	    type = BibtexEntryType.getType((String)i.next());
	    toSet = Util.nCase(type.getName());
	    if (type instanceof CustomEntryType)
		toSet = toSet + " *";
	    types_cb.addItem(toSet);

	}
    }

    void makeButtons(){
	ok = new JButton(Globals.lang("Store"));
	cancel=new JButton(Globals.lang("Close"));
	delete = new JButton(Globals.lang("Delete custom"));
        
	importTypes = new JButton(Globals.lang("Import"));
	exportTypes = new JButton(Globals.lang("Export"));
        buttonPanel.add( ok );
	buttonPanel.add(delete);
        buttonPanel.add(Box.createHorizontalStrut(5));
        
	buttonPanel.add(importTypes);
	buttonPanel.add(exportTypes);
        buttonPanel.add(Box.createHorizontalStrut(5));
	buttonPanel.add( cancel);
	ok.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    save();
		}
	    });
	cancel.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    dispose();
		}
	    });
        
	delete.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    BibtexEntryType type = BibtexEntryType
			.getType(name.getText());
		    if (type == null)
			messageLabel.setText(Globals.lang("There is no entry type")+
					     " '"+Util.nCase(name.getText())+
					     "' "+Globals.lang("defined."));
		    else if (!(type instanceof CustomEntryType))
			messageLabel.setText("'"+type.getName()+"' "+
					     Globals.lang("is a standard type."));
		    else {
			String nm = name.getText();
			if (BibtexEntryType.getStandardType(nm) == null) {
			    int reply = JOptionPane.showConfirmDialog
				(parent, Globals.lang("All entries of this "
						      +"type will be declared "
						      +"typeless. Continue?"),
				 Globals.lang("Delete custom format")+
				 " '"+Util.nCase(nm)+"'", JOptionPane.YES_NO_OPTION,
				 JOptionPane.WARNING_MESSAGE);
			    if (reply != JOptionPane.YES_OPTION)
				return;
			}
			BibtexEntryType.removeType(nm);
			setTypeSelection();
			updateTypesForEntries(Util.nCase(nm));
			messageLabel.setText
			    (Globals.lang("Removed entry type."));
		    }
		}
	    });

	exportTypes.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    String filename = Globals.getNewFile
			(parent, new File(System.getProperty("user.home")),
			 ".txt", JFileChooser.SAVE_DIALOG, false);
		    if (filename == null) 
			return;
		    File file = new File(filename);
		    if (!file.exists() ||  
			(JOptionPane.showConfirmDialog
                         (ths, "'"+file.getName()+"' "+Globals.lang("exists. Overwrite file?"),
                          Globals.lang("Export entry types"), JOptionPane.OK_CANCEL_OPTION)
                         == JOptionPane.OK_OPTION)) {

			try {
			    FileWriter out = new FileWriter(file);
			    Iterator i=BibtexEntryType.ALL_TYPES.keySet().iterator();
			    while (i.hasNext()) {
				Object o=BibtexEntryType.ALL_TYPES.get(i.next());
				if (o instanceof CustomEntryType) {
				    
				    ((CustomEntryType)o).save(out);
				}
			    }
			    out.close();
			} catch (IOException ex) {
			    JOptionPane.showMessageDialog
				(ths, Globals.lang("Could not export entry types")+": "+ex.getMessage(), Globals.lang("Export preferences"), JOptionPane.ERROR_MESSAGE);
			    
			}
		    }

		}
	    });

	importTypes.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    String filename = Globals.getNewFile
			(parent, new File(System.getProperty("user.home")),
			 ".txt", JFileChooser.OPEN_DIALOG, false);
		    if (filename == null) 
			return;

		    try {
			BufferedReader in = new BufferedReader(new FileReader(new File(filename)));
			String line;
			int count = 0;
			while ((line = in.readLine()) != null) {
			    line = line.trim();
			    if ((line.length() > 9+GUIGlobals.ENTRYTYPE_FLAG.length())
				&& line.substring(0, 9+GUIGlobals.ENTRYTYPE_FLAG.length()).equals("@comment{"+GUIGlobals.ENTRYTYPE_FLAG)
				&& line.substring(line.length()-1).equals("}")) {
				
				CustomEntryType type = CustomEntryType.parseEntryType(line.substring(9, line.length()-1));
				if (type != null) {
				    
				    BibtexEntryType.ALL_TYPES.put(type.getName().toLowerCase(), type);
				    count++;
				}
			    }

			    if (count > 0) {
				setTypeSelection();
				req_ta.setText("");
				opt_ta.setText("");
				name.setText("");
				messageLabel.setText(Globals.lang("Imported entry types")+": "+count);
			    }
			}
		    } catch (IOException ex) {
			JOptionPane.showMessageDialog
			    (ths, Globals.lang("Could not import entry types")+": "+ex.getMessage(), Globals.lang("Import entry types"), JOptionPane.ERROR_MESSAGE);
			
		    }
		}

		
	    });
    }

    
    private void updateTypesForEntries(String typeName) {
	if (parent.tabbedPane.getTabCount() == 0)
	    return;
	messageLabel.setText(Globals.lang("Updating entries..."));
	BibtexDatabase base;
	Iterator iter;
	for (int i=0; i<parent.tabbedPane.getTabCount(); i++) {
	    BasePanel bp = (BasePanel)parent.tabbedPane.getComponentAt(i);
	    boolean anyChanges = false;
	    bp.entryEditors.remove(typeName);
	    
	    base = bp.database;
	    iter = base.getKeySet().iterator();
	    while (iter.hasNext()) {
		anyChanges = anyChanges |
		    !(base.getEntryById((String)iter.next())).updateType();
	    }
	    if (anyChanges) {
		bp.markBaseChanged();
	    }
	}
    }

    public void itemStateChanged(ItemEvent e) {
	if (types_cb.getSelectedIndex() > 0) {
	    
	    String name = (String)types_cb.getSelectedItem();
	    updateToType((name.split(" "))[0]);
	} else {
	    name.setText("");
	    req_ta.setText("");
	    opt_ta.setText("");
	    name.requestFocus();
	}
    }

    public void updateToType(String o) {

	BibtexEntryType type = BibtexEntryType.getType(o);
	name.setText(type.getName());
	req_ta.setText(Util.stringArrayToDelimited
		       (type.getRequiredFields(), ";\n"));
	opt_ta.setText(Util.stringArrayToDelimited
		       (type.getOptionalFields(), ";\n"));

	req_ta.requestFocus();
    }
}
