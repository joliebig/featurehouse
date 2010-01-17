
package net.sf.jabref;

import java.awt.*;
import java.util.regex.Pattern;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;


public class FieldTextArea extends JTextArea implements FieldEditor {

	Dimension PREFERRED_SIZE;

	JScrollPane sp;

	FieldNameLabel label;

	String fieldName;

	final static Pattern bull = Pattern.compile("\\s*[-\\*]+.*");

	final static Pattern indent = Pattern.compile("\\s+.*");

	final boolean antialias = Globals.prefs.getBoolean("antialias");

	public FieldTextArea(String fieldName_, String content) {
		super(content);

        updateFont();
                
		
		
		addFocusListener(Globals.focusListener);
		addFocusListener(new FieldEditorFocusListener());
		sp = new JScrollPane(this, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		sp.setMinimumSize(new Dimension(200, 1));

		setLineWrap(true);
		setWrapStyleWord(true);
		fieldName = fieldName_;

		label = new FieldNameLabel(" " + Util.nCase(fieldName) + " ");
		setBackground(GUIGlobals.validFieldBackground);

		FieldTextMenu popMenu = new FieldTextMenu(this);
		this.addMouseListener(popMenu);
		label.addMouseListener(popMenu);
	}

	public Dimension getPreferredScrollableViewportSize() {
		return getPreferredSize();
	}

	public void paint(Graphics g) {
		Graphics2D g2 = (Graphics2D) g;
		if (antialias)
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		super.paint(g2);
	}

	public String getFieldName() {
		return fieldName;
	}

	public void setFieldName(String newName) {
		fieldName = newName;
	}

	public JLabel getLabel() {
		return label;
	}

	public void setLabelColor(Color c) {
		label.setForeground(c);
	}

	public JComponent getPane() {
		return sp;
	}

	public JComponent getTextComponent() {
		return this;
	}


    public void updateFont() {
        setFont(GUIGlobals.CURRENTFONT);
    }

    public void paste(String textToInsert) {
		int sel = getSelectionEnd() - getSelectionStart();
		if (sel > 0) 
			replaceSelection(textToInsert);
		else {
			int cPos = this.getCaretPosition();
			this.insert(textToInsert, cPos);
		}
	}

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}
