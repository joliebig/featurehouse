

package net.sf.jabref.groups; 

import java.awt.datatransfer.DataFlavor; 
import java.awt.datatransfer.Transferable; 
import java.awt.datatransfer.UnsupportedFlavorException; 
import java.io.IOException; 
import java.util.Enumeration; 
import java.util.Vector; 

import javax.swing.tree.DefaultMutableTreeNode; 
import javax.swing.tree.TreeNode; 
import javax.swing.undo.AbstractUndoableEdit; 

import net.sf.jabref.BibtexDatabase; 
import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.SearchRule; 


public  class  GroupTreeNode  extends DefaultMutableTreeNode implements 
		Transferable {
	
	public static final DataFlavor flavor;

	
	public static final DataFlavor[] flavors;

	

	static {
		DataFlavor df = null;
		try {
			df = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType
					+ ";class=net.sf.jabref.groups.GroupTreeNode");
		} catch (ClassNotFoundException e) {
			
		}
		flavor = df;
		flavors = new DataFlavor[] { flavor };
	}

	

	
	public GroupTreeNode(AbstractGroup group) {
		setGroup(group);
	}


	

	
	public AbstractGroup getGroup() {
		return (AbstractGroup) getUserObject();
	}


	

	
	public void setGroup(AbstractGroup group) {
		setUserObject(group);
	}


	

	
	public String getTreeAsString() {
		StringBuffer sb = new StringBuffer();
		Enumeration<GroupTreeNode> e = preorderEnumeration();
		GroupTreeNode cursor;
		while (e.hasMoreElements()) {
			cursor = e.nextElement();
            sb.append(cursor.getLevel()).append(" ").append(cursor.getGroup().toString()).append("\n");
		}
		return sb.toString();
	}


	

	
	public GroupTreeNode deepCopy() {
		GroupTreeNode copy = new GroupTreeNode(getGroup());
		for (int i = 0; i < getChildCount(); ++i)
			copy.add(((GroupTreeNode) getChildAt(i)).deepCopy());
		return copy;
	}


	
        
        
        public void refreshGroupsForNewDatabase(BibtexDatabase db) {
            for (int i = 0; i < getChildCount(); ++i) {
                GroupTreeNode node = (GroupTreeNode)getChildAt(i);
                node.getGroup().refreshForNewDatabase(db);
                node.refreshGroupsForNewDatabase(db);
            }
        }


	
      
	
	public int[] getIndexedPath() {
		TreeNode[] path = getPath();
		int[] indexedPath = new int[path.length - 1];
		for (int i = 1; i < path.length; ++i)
			indexedPath[i - 1] = path[i - 1].getIndex(path[i]);
		return indexedPath;
	}


	

	
	public GroupTreeNode getNode(int[] indexedPath) {
		GroupTreeNode cursor = this;
		for (int i = 0; i < indexedPath.length; ++i)
			cursor = (GroupTreeNode) cursor.getChildAt(indexedPath[i]);
		return cursor;
	}


	

	
	public GroupTreeNode getDescendant(int[] indexedPath) {
		GroupTreeNode cursor = this;
		for (int i = 0; i < indexedPath.length && cursor != null; ++i)
			cursor = (GroupTreeNode) cursor.getChildAt(indexedPath[i]);
		return cursor;
	}


	

	
	public SearchRule getSearchRule() {
		return getSearchRule(getGroup().getHierarchicalContext());
	}


	

	protected SearchRule getSearchRule(int originalContext) {
		final int context = getGroup().getHierarchicalContext();
		if (context == AbstractGroup.INDEPENDENT)
			return getGroup().getSearchRule();
		AndOrSearchRuleSet searchRule = new AndOrSearchRuleSet(
				context == AbstractGroup.REFINING, false);
		searchRule.addRule(getGroup().getSearchRule());
		if (context == AbstractGroup.INCLUDING
				&& originalContext != AbstractGroup.REFINING) {
			for (int i = 0; i < getChildCount(); ++i)
				searchRule.addRule(((GroupTreeNode) getChildAt(i))
						.getSearchRule(originalContext));
		} else if (context == AbstractGroup.REFINING && !isRoot()
				&& originalContext != AbstractGroup.INCLUDING) {
			searchRule.addRule(((GroupTreeNode) getParent())
					.getSearchRule(originalContext));
		}
		return searchRule;
	}


	

	@Override
	@SuppressWarnings("unchecked")
	public Enumeration<GroupTreeNode> preorderEnumeration(){
		return super.preorderEnumeration();
	}


	
	
	@Override
	@SuppressWarnings("unchecked")
	public Enumeration<GroupTreeNode> depthFirstEnumeration(){
		return super.depthFirstEnumeration();
	}


	
	
	@Override
	@SuppressWarnings("unchecked")
	public Enumeration<GroupTreeNode> breadthFirstEnumeration(){
		return super.breadthFirstEnumeration();
	}


	
	
	@Override
	@SuppressWarnings("unchecked")
	public Enumeration<GroupTreeNode> children(){
		return super.children();
	}


	
	
	
	public AbstractGroup[] getMatchingGroups(BibtexEntry entry) {
		Vector<AbstractGroup> matchingGroups = new Vector<AbstractGroup>();
		Enumeration<GroupTreeNode> e = preorderEnumeration();
		AbstractGroup group;
		while (e.hasMoreElements()) {
			group = (e.nextElement()).getGroup();
			if (group.contains(null, entry)) 
				matchingGroups.add(group);
		}
		AbstractGroup[] matchingGroupsArray = new AbstractGroup[matchingGroups
				.size()];
		return matchingGroups.toArray(matchingGroupsArray);
	}


	

	public boolean canMoveUp() {
		return getPreviousSibling() != null
				&& !(getGroup() instanceof AllEntriesGroup);
	}


	

	public boolean canMoveDown() {
		return getNextSibling() != null
				&& !(getGroup() instanceof AllEntriesGroup);
	}


	

	public boolean canMoveLeft() {
		return !(getGroup() instanceof AllEntriesGroup)
				&& !(((GroupTreeNode) getParent()).getGroup() instanceof AllEntriesGroup);
	}


	

	public boolean canMoveRight() {
		return getPreviousSibling() != null
				&& !(getGroup() instanceof AllEntriesGroup);
	}


	

	public AbstractUndoableEdit moveUp(GroupSelector groupSelector) {
		final GroupTreeNode myParent = (GroupTreeNode) getParent();
		final int index = myParent.getIndex(this);
		if (index > 0) {
			UndoableMoveGroup undo = new UndoableMoveGroup(groupSelector,
					groupSelector.getGroupTreeRoot(), this, myParent, index - 1);
			myParent.insert(this, index - 1);
			return undo;
		}
		return null;
	}


	

	public AbstractUndoableEdit moveDown(GroupSelector groupSelector) {
		final GroupTreeNode myParent = (GroupTreeNode) getParent();
		final int index = myParent.getIndex(this);
		if (index < parent.getChildCount() - 1) {
			UndoableMoveGroup undo = new UndoableMoveGroup(groupSelector,
					groupSelector.getGroupTreeRoot(), this, myParent, index + 1);
			myParent.insert(this, index + 1);
			return undo;
		}
		return null;
	}


	

	public AbstractUndoableEdit moveLeft(GroupSelector groupSelector) {
		final GroupTreeNode myParent = (GroupTreeNode) getParent();
		final GroupTreeNode myGrandParent = (GroupTreeNode) myParent
				.getParent();
		
		if (myGrandParent == null)
			return null;
		final int index = myGrandParent.getIndex(myParent);
		UndoableMoveGroup undo = new UndoableMoveGroup(groupSelector,
				groupSelector.getGroupTreeRoot(), this, myGrandParent,
				index + 1);
		myGrandParent.insert(this, index + 1);
		return undo;
	}


	

	public AbstractUndoableEdit moveRight(GroupSelector groupSelector) {
		final GroupTreeNode myPreviousSibling = (GroupTreeNode) getPreviousSibling();
		
		if (myPreviousSibling == null)
			return null;
		UndoableMoveGroup undo = new UndoableMoveGroup(groupSelector,
				groupSelector.getGroupTreeRoot(), this, myPreviousSibling,
				myPreviousSibling.getChildCount());
		myPreviousSibling.add(this);
		return undo;
	}


	

	
	public GroupTreeNode getChildAt(int[] path) {
		GroupTreeNode cursor = this;
		for (int i = 0; i < path.length && cursor != null; ++i)
			cursor = (GroupTreeNode) cursor.getChildAt(path[i]);
		return cursor;
	}


	

	
	public AbstractUndoableEdit addToGroup(BibtexEntry[] entries) {
		if (getGroup() == null)
			return null; 
		AbstractUndoableEdit undo = getGroup().add(entries);
		if (undo instanceof UndoableChangeAssignment)
			((UndoableChangeAssignment) undo).setEditedNode(this);
		return undo;
	}


	

	
	public AbstractUndoableEdit removeFromGroup(BibtexEntry[] entries) {
		if (getGroup() == null)
			return null; 
		AbstractUndoableEdit undo = getGroup().remove(entries);
		if (undo instanceof UndoableChangeAssignment)
			((UndoableChangeAssignment) undo).setEditedNode(this);
		return undo;
	}


	

	public DataFlavor[] getTransferDataFlavors() {
		return flavors;
	}


	

	public boolean isDataFlavorSupported(DataFlavor someFlavor) {
		return someFlavor.equals(GroupTreeNode.flavor);
	}


	

	public Object getTransferData(DataFlavor someFlavor)
			throws UnsupportedFlavorException, IOException {
		if (!isDataFlavorSupported(someFlavor))
			throw new UnsupportedFlavorException(someFlavor);
		return this;
	}


	

	
	public boolean equals(Object other) {
		if (!(other instanceof GroupTreeNode))
			return false;
		final GroupTreeNode otherNode = (GroupTreeNode) other;
		if (getChildCount() != otherNode.getChildCount())
			return false;
		AbstractGroup g1 = getGroup();
		AbstractGroup g2 = otherNode.getGroup();
		if ((g1 == null && g2 != null) || (g1 != null && g2 == null))
			return false;
		if (g1 != null && g2 != null && !g1.equals(g2))
			return false;
		for (int i = 0; i < getChildCount(); ++i) {
			if (!getChildAt(i).equals(otherNode.getChildAt(i)))
				return false;
		}
		return true;
	}


	

	static {
		DataFlavor df = null;
		try {
			df = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType
					+ ";class=net.sf.jabref.groups.GroupTreeNode");
		} catch (ClassNotFoundException e) {
			
		}
		flavor = df;
		flavors = new DataFlavor[] { flavor };
	}


}
