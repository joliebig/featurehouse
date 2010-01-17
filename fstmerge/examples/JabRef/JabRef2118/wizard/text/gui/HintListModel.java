package net.sf.jabref.wizard.text.gui; 

import javax.swing.DefaultListModel; 
import javax.swing.table.AbstractTableModel; 
import javax.swing.table.TableModel; 
import javax.swing.event.TableModelListener; 
import java.util.*; 

import java.util.Vector; 

import net.sf.jabref.wizard.integrity.IntegrityMessage; 

public  class  HintListModel  extends DefaultListModel {
	
  


	

	public void valueUpdated(int index) {
		super.fireContentsChanged(this, index, index);
	}


	
	
	public void setData(Vector<IntegrityMessage> newData) {
		clear();
		if (newData != null) {
			for (IntegrityMessage message : newData){
				addElement(message);
			}
		}
	}


}
