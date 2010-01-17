
package net.sf.jabref.groups; 

import javax.swing.undo.AbstractUndoableEdit; 

import net.sf.jabref.Globals; 

public  class  UndoableModifyGroup  extends AbstractUndoableEdit {
	
    private final GroupSelector m_groupSelector;

	
    private final AbstractGroup m_oldGroupBackup;

	
    private final AbstractGroup m_newGroupBackup;

	
    private final GroupTreeNode m_groupsRootHandle;

	
    private final int[] m_pathToNode;

	

    
    public UndoableModifyGroup(GroupSelector gs, GroupTreeNode groupsRoot,
            GroupTreeNode node, AbstractGroup newGroup) {
        m_groupSelector = gs;
        m_oldGroupBackup = node.getGroup().deepCopy();
        m_newGroupBackup = newGroup.deepCopy();
        m_pathToNode = node.getIndexedPath();
        m_groupsRootHandle = groupsRoot;
    }


	

    public String getUndoPresentationName() {
        return Globals.lang("Undo") + ": " 
            + Globals.lang("modify group");
    }


	

    public String getRedoPresentationName() {
        return Globals.lang("Redo") + ": " 
            + Globals.lang("modify group");
    }


	

    public void undo() {
        super.undo();
        m_groupsRootHandle.getDescendant(m_pathToNode).setGroup(
                m_oldGroupBackup.deepCopy());
        m_groupSelector.revalidateGroups();
    }


	

    public void redo() {
        super.redo();
        m_groupsRootHandle.getDescendant(m_pathToNode).setGroup(
                m_newGroupBackup.deepCopy());
        m_groupSelector.revalidateGroups();
    }



}
