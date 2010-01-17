

package net.sf.jabref; 

import java.awt.Color; 
import java.awt.Container; 

import javax.swing.JComponent; 
import javax.swing.JLabel; 



public  interface  FieldEditor {
	

	public String getFieldName();


	

	
	public JComponent getPane();


	

	
	public JComponent getTextComponent();


	

	public JLabel getLabel();


	

	public void setLabelColor(Color c);


	

	public void setBackground(Color c);


	

	public String getText();


	

	
	public void setText(String newText);


	

	public void append(String text);


	

	public Container getParent();


	

	public void requestFocus();


	

	public void setEnabled(boolean enabled);


	

    public void updateFont();


	
    
	public void paste(String textToInsert);


	

	
	public String getSelectedText();



}
