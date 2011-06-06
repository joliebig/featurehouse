package org.gjt.sp.jedit.gui;

import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;

import org.gjt.sp.jedit.textarea.JEditTextArea;


abstract public class DynamicContextMenuService {
	
	public abstract JMenuItem[] createMenu(JEditTextArea ta, MouseEvent evt); 

}
