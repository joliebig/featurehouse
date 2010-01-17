
package net.sf.jabref; 

import java.awt.*; 
import java.awt.event.FocusEvent; 
import java.awt.event.FocusListener; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.List; 

import javax.swing.*; 
import javax.swing.event.DocumentEvent; 
import javax.swing.event.DocumentListener; 
import javax.swing.text.JTextComponent; 

import net.sf.jabref.gui.AutoCompleteListener; 
import net.sf.jabref.gui.AutoCompleter; 
import net.sf.jabref.gui.FileListEditor; 

import com.jgoodies.forms.builder.DefaultFormBuilder; 
import com.jgoodies.forms.layout.FormLayout; 


public  class  EntryEditorTab {
	

	private JPanel panel = new JPanel();

	

	private String[] fields;

	

	private EntryEditor parent;

	

	private HashMap<String, FieldEditor> editors = new HashMap<String, FieldEditor>();

	

	private FieldEditor activeField = null;

	

	public EntryEditorTab(JabRefFrame frame, BasePanel panel, List<String> fields, EntryEditor parent,
                          boolean addKeyField, String name) {
		if (fields != null)
			this.fields = fields.toArray(new String[0]);
		else
			this.fields = new String[] {};

		this.parent = parent;

		setupPanel(frame, panel, addKeyField, name);

		
		panel.setFocusCycleRoot(true);
	}


	


    void setupPanel(JabRefFrame frame, BasePanel bPanel, boolean addKeyField, String title) {
    	
    	InputMap im = panel.getInputMap(JComponent.WHEN_FOCUSED);
		ActionMap am = panel.getActionMap();

		im.put(Globals.prefs.getKey("Entry editor, previous entry"), "prev");
		am.put("prev", parent.prevEntryAction);
		im.put(Globals.prefs.getKey("Entry editor, next entry"), "next");
		am.put("next", parent.nextEntryAction);

		im.put(Globals.prefs.getKey("Entry editor, store field"), "store");
		am.put("store", parent.storeFieldAction);
		im.put(Globals.prefs.getKey("Entry editor, next panel"), "right");
		im.put(Globals.prefs.getKey("Entry editor, next panel 2"), "right");
		am.put("left", parent.switchLeftAction);
		im.put(Globals.prefs.getKey("Entry editor, previous panel"), "left");
		im.put(Globals.prefs.getKey("Entry editor, previous panel 2"), "left");
		am.put("right", parent.switchRightAction);
		im.put(Globals.prefs.getKey("Help"), "help");
		am.put("help", parent.helpAction);
		im.put(Globals.prefs.getKey("Save database"), "save");
		am.put("save", parent.saveDatabaseAction);
		im.put(Globals.prefs.getKey("Next tab"), "nexttab");
		am.put("nexttab", parent.frame.nextTab);
		im.put(Globals.prefs.getKey("Previous tab"), "prevtab");
		am.put("prevtab", parent.frame.prevTab);
    	
    	  	
        panel.setName(title);
        
        String colSpec = "fill:pref, 1dlu, fill:pref:grow, 1dlu, fill:pref";
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < fields.length; i++) {
            sb.append("fill:pref:grow, ");
        }
        if (addKeyField)
            sb.append("4dlu, fill:pref");
        else
            sb.delete(sb.length()-2, sb.length());
        String rowSpec = sb.toString();

        DefaultFormBuilder builder = new DefaultFormBuilder
                (new FormLayout(colSpec, rowSpec), panel);

        for (int i = 0; i < fields.length; i++) {
            
            int editorType = BibtexFields.getEditorType(fields[i]);

            final FieldEditor ta;
            if (editorType == GUIGlobals.FILE_LIST_EDITOR)
                ta = new FileListEditor(frame, bPanel.metaData(), fields[i], null, parent);
            else
                ta = new FieldTextArea(fields[i], null);
            
            
            JComponent ex = parent.getExtra(fields[i], ta);

            
            AutoCompleter autoComp = bPanel.getAutoCompleter(fields[i]);
            AutoCompleteListener acl = null;
            if (autoComp != null) {
                acl = new AutoCompleteListener(autoComp);
            }
            setupJTextComponent(ta.getTextComponent(), acl);

            
            editors.put(fields[i], ta);
            if (i == 0)
                activeField = ta;
            
            ta.getPane().setPreferredSize(new Dimension(100,
                    (int)(50.0*BibtexFields.getFieldWeight(fields[i]))));
            builder.append(ta.getLabel());
            if (ex == null)
                builder.append(ta.getPane(), 3);
            else {
                builder.append(ta.getPane());
                JPanel pan = new JPanel();
                pan.setLayout(new BorderLayout());
                pan.add(ex, BorderLayout.NORTH);
                builder.append(pan);
            }
            builder.nextLine();
        }

        
		if (addKeyField) {
			final FieldTextField tf = new FieldTextField(BibtexFields.KEY_FIELD, parent
				.getEntry().getField(BibtexFields.KEY_FIELD), true);
<<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_40486
            
			setupJTextComponent(tf, null);
=======
            
			setupJTextComponent(tf);
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_40488

			editors.put("bibtexkey", tf);
			
			if (editors.size() == 1)
				activeField = tf;
            builder.nextLine();
			builder.append(tf.getLabel());
			builder.append(tf, 3);
		}
    }


	


	BibtexEntry entry;

	

	public BibtexEntry getEntry() {
		return entry;
	}


	

	boolean isFieldModified(FieldEditor f) {
		String text = f.getText().trim();

		if (text.length() == 0) {
			return getEntry().getField(f.getFieldName()) != null;
		} else {
			Object entryValue = getEntry().getField(f.getFieldName());
			return entryValue == null || !entryValue.toString().equals(text);
		}
	}


	

	public void markIfModified(FieldEditor f) {
		
		
		if (!updating && !parent.panel.isBaseChanged() && isFieldModified(f)) {
			markBaseChanged();
		}
	}


	

	void markBaseChanged() {
		parent.panel.markBaseChanged();
	}


	

	
	public void setActive(FieldEditor c) {
		activeField = c;
	}


	

	public FieldEditor getActive() {
		return activeField;
	}


	

	public List<String> getFields() {
		return java.util.Arrays.asList(fields);
	}


	

	public void activate() {
		if (activeField != null){
			
			new FocusRequester(activeField.getTextComponent());
		}
	}


	

	
	public void updateAll() {
		setEntry(getEntry());
	}


	

	protected boolean updating = false;

	

	public void setEntry(BibtexEntry entry) {
		try {
			updating = true;
			Iterator<FieldEditor> i = editors.values().iterator();
			while (i.hasNext()) {
				FieldEditor editor = i.next();
				Object content = entry.getField(editor.getFieldName());
                String toSet = (content == null) ? "" : content.toString();
                if (!toSet.equals(editor.getText()))
				    editor.setText(toSet);
			}
			this.entry = entry;
		} finally {
			updating = false;
		}
	}


	

	public boolean updateField(String field, String content) {
		if (!editors.containsKey(field))
			return false;
		FieldEditor ed = editors.get(field);
		ed.setText(content);
		return true;
	}


	

	public void validateAllFields() {
		for (Iterator<String> i = editors.keySet().iterator(); i.hasNext();) {
			String field = i.next();
			FieldEditor ed = editors.get(field);
			ed.setEnabled(true);
			if (((Component) ed).hasFocus())
				ed.setBackground(GUIGlobals.activeEditor);
			else
				ed.setBackground(GUIGlobals.validFieldBackground);
		}
	}


	

	public void setEnabled(boolean enabled) {
		Iterator<FieldEditor> i = editors.values().iterator();
		while (i.hasNext()) {
			FieldEditor editor = i.next();
			editor.setEnabled(enabled);
		}
	}


	

	public Component getPane() {
		return panel;
	}


	

	
	


	

	
	FocusListener fieldListener = new FocusListener() {
	
		JTextComponent c;

		DocumentListener d;

		public void focusGained(FocusEvent e) {

			synchronized (this){
				if (c != null) {
					c.getDocument().removeDocumentListener(d);
					c = null;
					d = null;
				}

				if (e.getSource() instanceof JTextComponent) {

					c = (JTextComponent) e.getSource();
					
					d = new DocumentListener() {

						void fire(DocumentEvent e) {
							if (c.isFocusOwner()) {
								markIfModified((FieldEditor) c);
							}
						}

						public void changedUpdate(DocumentEvent e) {
							fire(e);
						}

						public void insertUpdate(DocumentEvent e) {
							fire(e);
						}

						public void removeUpdate(DocumentEvent e) {
							fire(e);
						}
					};
					c.getDocument().addDocumentListener(d);
				}
			}

			setActive((FieldEditor) e.getSource());

		}

		public void focusLost(FocusEvent e) {
            synchronized (this) {
				if (c != null) {
					c.getDocument().removeDocumentListener(d);
					c = null;
					d = null;
				}
			}
			if (!e.isTemporary())
				parent.updateField(e.getSource());
		}
	};

	

	
	public void setupJTextComponent(final JComponent component, final AutoCompleteListener acl) {

        
        
        
        
        
        
        
        
        if (acl != null) {
            component.addKeyListener(acl);
            component.addFocusListener(acl);
            acl.setNextFocusListener(fieldListener);
        }
        else
		    component.addFocusListener(fieldListener);

		InputMap im = component.getInputMap(JComponent.WHEN_FOCUSED);
		ActionMap am = component.getActionMap();

		im.put(Globals.prefs.getKey("Entry editor, previous entry"), "prev");
		am.put("prev", parent.prevEntryAction);
		im.put(Globals.prefs.getKey("Entry editor, next entry"), "next");
		am.put("next", parent.nextEntryAction);

		im.put(Globals.prefs.getKey("Entry editor, store field"), "store");
		am.put("store", parent.storeFieldAction);
		im.put(Globals.prefs.getKey("Entry editor, next panel"), "right");
		im.put(Globals.prefs.getKey("Entry editor, next panel 2"), "right");
		am.put("left", parent.switchLeftAction);
		im.put(Globals.prefs.getKey("Entry editor, previous panel"), "left");
		im.put(Globals.prefs.getKey("Entry editor, previous panel 2"), "left");
		am.put("right", parent.switchRightAction);
		im.put(Globals.prefs.getKey("Help"), "help");
		am.put("help", parent.helpAction);
		im.put(Globals.prefs.getKey("Save database"), "save");
		am.put("save", parent.saveDatabaseAction);
		im.put(Globals.prefs.getKey("Next tab"), "nexttab");
		am.put("nexttab", parent.frame.nextTab);
		im.put(Globals.prefs.getKey("Previous tab"), "prevtab");
		am.put("prevtab", parent.frame.prevTab);

		try {
			HashSet<AWTKeyStroke> keys = new HashSet<AWTKeyStroke>(component
				.getFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS));
			keys.clear();
			keys.add(AWTKeyStroke.getAWTKeyStroke("pressed TAB"));
			component.setFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS, keys);
			keys = new HashSet<AWTKeyStroke>(component
				.getFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS));
			keys.clear();
			keys.add(KeyStroke.getKeyStroke("shift pressed TAB"));
			component.setFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS, keys);
		} catch (Throwable t) {
			System.err.println(t);
		}

    }


}
