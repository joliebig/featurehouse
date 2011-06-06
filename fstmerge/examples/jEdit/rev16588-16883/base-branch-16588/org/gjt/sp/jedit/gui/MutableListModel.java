

package org.gjt.sp.jedit.gui;

public interface MutableListModel extends javax.swing.ListModel
{
	public boolean removeElement(Object elem);
	public void insertElementAt(Object elem, int index);
}
