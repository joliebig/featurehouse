
package net.sf.jabref; 

import java.awt.Color; 
import java.awt.Graphics; 
import java.awt.Graphics2D; 
import java.awt.RenderingHints; 

import javax.swing.JComponent; 
import javax.swing.JLabel; 
import javax.swing.JTextField; 

public  class  FieldTextField  extends JTextField implements  FieldEditor {
	

	protected String fieldName;

	

	protected JLabel label;

	

	private boolean antialias = Globals.prefs.getBoolean("antialias");

	

	public FieldTextField(String fieldName_, String content, boolean changeColorOnFocus) {
		super(content);

        updateFont();

        
		
		
		addFocusListener(Globals.focusListener);
		if (changeColorOnFocus)
			addFocusListener(new FieldEditorFocusListener());
		fieldName = fieldName_;
		label = new FieldNameLabel(" " + Util.nCase(fieldName) + " ");
		
		
		setBackground(GUIGlobals.validFieldBackground);
		
		
		
		
		

		FieldTextMenu popMenu = new FieldTextMenu(this);
		this.addMouseListener(popMenu);
		label.addMouseListener(popMenu);
	}


	

	public void append(String text) {
		setText(getText() + text);
	}


	

	public String getFieldName() {
		return fieldName;
	}


	

	public JLabel getLabel() {
		return label;
	}


	

	public void setLabelColor(Color c) {
		label.setForeground(c);
		throw new NullPointerException("ok");
	}


	

	public JComponent getPane() {
		return this;
	}


	

	public JComponent getTextComponent() {
		return this;

    }


	

    public void updateFont() {
        setFont(GUIGlobals.CURRENTFONT);
    }


	

    public void paste(String textToInsert) {
		int sel = getSelectionEnd() - getSelectionStart();
		if (sel < 1) {
			int cPos = getCaretPosition();
			select(cPos, cPos);
		}
		replaceSelection(textToInsert);
	}


	

    



}
