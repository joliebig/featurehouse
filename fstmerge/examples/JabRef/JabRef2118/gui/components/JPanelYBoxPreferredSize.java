package net.sf.jabref.gui.components; 

import java.awt.*; 

import java.awt.Component; 
import java.awt.Dimension; 

public  class  JPanelYBoxPreferredSize  extends JPanelYBox {
	
	public JPanelYBoxPreferredSize() {
		
	}


	
	public JPanelYBoxPreferredSize(Component c) {
		add(c);
	}


	
	public Dimension getMaximumSize() {
		return getPreferredSize();
	}



}
