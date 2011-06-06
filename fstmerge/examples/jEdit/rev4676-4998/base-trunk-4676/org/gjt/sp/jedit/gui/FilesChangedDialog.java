

package org.gjt.sp.jedit.gui;


import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;



public class FilesChangedDialog extends EnhancedDialog
{
	
	public FilesChangedDialog(View view, int[] states)
	{
		super(view,jEdit.getProperty("files-changed.title"),false);

		this.view = view;

		JPanel content = new JPanel(new BorderLayout(12,12));
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		Box iconBox = new Box(BoxLayout.Y_AXIS);
		iconBox.add(new JLabel(UIManager.getIcon("OptionPane.warningIcon")));
		iconBox.add(Box.createGlue());
		content.add(BorderLayout.WEST,iconBox);

		JPanel centerPanel = new JPanel(new BorderLayout());

		JLabel label = new JLabel(jEdit.getProperty("files-changed.caption"));
		label.setBorder(new EmptyBorder(0,0,6,0));
		centerPanel.add(BorderLayout.NORTH,label);

		DefaultMutableTreeNode deleted = new DefaultMutableTreeNode(
			jEdit.getProperty("files-changed.deleted"),true);
		DefaultMutableTreeNode changed = new DefaultMutableTreeNode(
			jEdit.getProperty("files-changed.changed"),true);
		DefaultMutableTreeNode changedDirty = new DefaultMutableTreeNode(
			jEdit.getProperty("files-changed.changed-dirty"),true);
		Buffer[] buffers = jEdit.getBuffers();
		for(int i = 0; i < states.length; i++)
		{
			Buffer buffer = buffers[i];
			DefaultMutableTreeNode addTo;
			switch(states[i])
			{
			case Buffer.FILE_DELETED:
				addTo = deleted;
				break;
			case Buffer.FILE_CHANGED:
				addTo = (buffer.isDirty() ? changedDirty : changed);
				break;
			default:
				addTo = null;
				break;
			}

			if(addTo != null)
				addTo.add(new DefaultMutableTreeNode(buffer));
		}

		DefaultMutableTreeNode root = new DefaultMutableTreeNode("",true);
		if(deleted.getChildCount() != 0)
			root.add(deleted);
		if(changed.getChildCount() != 0)
			root.add(changed);
		if(changedDirty.getChildCount() != 0)
			root.add(changedDirty);

		bufferTree = new JTree(new DefaultTreeModel(root));
		bufferTree.setRootVisible(false);
		bufferTree.setVisibleRowCount(10);
		bufferTree.setCellRenderer(new Renderer());
		bufferTree.getSelectionModel().addTreeSelectionListener(new TreeHandler());
		bufferTree.getSelectionModel().setSelectionMode(
			ListSelectionModel.SINGLE_SELECTION);

		centerPanel.add(BorderLayout.CENTER,new JScrollPane(bufferTree));

		content.add(BorderLayout.CENTER,centerPanel);

		Box buttons = new Box(BoxLayout.X_AXIS);
		buttons.add(Box.createGlue());
		JButton close = new JButton(jEdit.getProperty("common.close"));
		buttons.add(close);
		close.addActionListener(new ActionHandler());
		buttons.add(Box.createGlue());

		content.add(BorderLayout.SOUTH,buttons);

		bufferTree.expandPath(new TreePath(
			new Object[] {
				root,
				deleted
			}));
		bufferTree.expandPath(new TreePath(
			new Object[] {
				root,
				changed
			}));
		bufferTree.expandPath(new TreePath(
			new Object[] {
				root,
				changedDirty
			}));

		GUIUtilities.requestFocus(this,bufferTree);

		pack();
		setLocationRelativeTo(view);
		show();
	} 

	
	public void ok()
	{
		dispose();
	} 

	
	public void cancel()
	{
		dispose();
	} 

	
	private View view;
	private JTree bufferTree;
	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			dispose();
		}
	} 

	
	class TreeHandler implements TreeSelectionListener
	{
		public void valueChanged(TreeSelectionEvent evt)
		{
			TreePath path = bufferTree.getSelectionPath();
			if(path != null)
			{
				DefaultMutableTreeNode node = (DefaultMutableTreeNode)path.getLastPathComponent();
				if(node.getUserObject() instanceof Buffer)
					view.goToBuffer((Buffer)node.getUserObject());
			}
		}
	} 

	
	class Renderer extends DefaultTreeCellRenderer
	{
		public Renderer()
		{
			entryFont = UIManager.getFont("Tree.font");
			if(entryFont == null)
				entryFont = jEdit.getFontProperty("metal.secondary.font");
			groupFont = entryFont.deriveFont(Font.BOLD);
		}

		public Component getTreeCellRendererComponent(JTree tree,
			Object value, boolean selected, boolean expanded,
			boolean leaf, int row, boolean hasFocus)
		{
			super.getTreeCellRendererComponent(tree,value,
				selected,expanded,leaf,row,hasFocus);

			DefaultMutableTreeNode node = (DefaultMutableTreeNode)value;

			if (node.getUserObject() instanceof String)
				this.setFont(groupFont);
			else
				this.setFont(entryFont);

			setIcon(null);

			return this;
		}

		private Font entryFont;
		private Font groupFont;
	} 
}
