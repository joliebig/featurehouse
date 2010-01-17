
package net.sf.jabref.groups; 

import javax.swing.undo.AbstractUndoableEdit; 

import net.sf.jabref.Globals; 

 

class  UndoableAddOrRemoveGroup  extends AbstractUndoableEdit {
	
    
    private final GroupTreeNode m_groupsRootHandle;

	
    
    private final GroupTreeNode m_subtreeBackup;

	
    
    private final int m_subtreeRootChildCount;

	
    
    private final int[] m_pathToNode;

	
    
    private final int m_editType;

	
    private final GroupSelector m_groupSelector;

	
    private boolean m_revalidate = true;

	
    
    public static final int ADD_NODE = 0;

	
    
    public static final int REMOVE_NODE_KEEP_CHILDREN = 1;

	
    
    public static final int REMOVE_NODE_AND_CHILDREN = 2;

	

    
    public UndoableAddOrRemoveGroup(GroupSelector gs, GroupTreeNode groupsRoot,
            GroupTreeNode editedNode, int editType) {
        m_groupSelector = gs;
        m_groupsRootHandle = groupsRoot;
        m_editType = editType;
        m_subtreeRootChildCount = editedNode.getChildCount();
        
        
        m_subtreeBackup = editType != REMOVE_NODE_KEEP_CHILDREN ? editedNode
                .deepCopy() : new GroupTreeNode(editedNode.getGroup().deepCopy());
        
        
        
        m_pathToNode = editedNode.getIndexedPath();
    }


	

    public String getUndoPresentationName() {
        return Globals.lang("Undo") + ": " + getName();
    }


	

    public String getName() {
        switch (m_editType) {
        case ADD_NODE:
            return Globals.lang("add group");
        case REMOVE_NODE_KEEP_CHILDREN:
            return Globals.lang("remove group (keep subgroups)");
        case REMOVE_NODE_AND_CHILDREN:
            return Globals.lang("remove group and subgroups");
        }
        return "? (" + Globals.lang("unknown edit") + ")";
    }


	

    public String getRedoPresentationName() {
        return Globals.lang("Redo") + ": " + getName();
    }


	

    public void undo() {
        super.undo();
        doOperation(true);
    }


	

    public void redo() {
        super.redo();
        doOperation(false);
    }


	

    private void doOperation(boolean undo) {
        GroupTreeNode cursor = m_groupsRootHandle;
        final int childIndex = m_pathToNode[m_pathToNode.length - 1];
        
        for (int i = 0; i < m_pathToNode.length - 1; ++i)
            cursor = (GroupTreeNode) cursor.getChildAt(m_pathToNode[i]);
        if (undo) {
            switch (m_editType) {
            case ADD_NODE:
                cursor.remove(childIndex);
                break;
            case REMOVE_NODE_KEEP_CHILDREN:
                
                GroupTreeNode newNode = m_subtreeBackup.deepCopy();
                for (int i = childIndex; i < childIndex
                        + m_subtreeRootChildCount; ++i) {
                    newNode.add((GroupTreeNode) cursor.getChildAt(childIndex));
                }
                cursor.insert(newNode, childIndex);
                break;
            case REMOVE_NODE_AND_CHILDREN:
                cursor.insert(m_subtreeBackup.deepCopy(), childIndex);
                break;
            }
        } else { 
            switch (m_editType) {
            case ADD_NODE:
                cursor.insert(m_subtreeBackup.deepCopy(), childIndex);
                break;
            case REMOVE_NODE_KEEP_CHILDREN:
                
                GroupTreeNode removedNode = (GroupTreeNode) cursor
                        .getChildAt(childIndex);
                cursor.remove(childIndex);
                while (removedNode.getChildCount() > 0)
                    cursor.insert((GroupTreeNode) removedNode.getFirstChild(),
                            childIndex);
                break;
            case REMOVE_NODE_AND_CHILDREN:
                cursor.remove(childIndex);
                break;
            }
        }
        if (m_revalidate)
            m_groupSelector.revalidateGroups();
    }


	

    
    public void setRevalidate(boolean val) {
        m_revalidate = val;
    }



}
