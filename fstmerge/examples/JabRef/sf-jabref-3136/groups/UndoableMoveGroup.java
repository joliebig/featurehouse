

package net.sf.jabref.groups;

import javax.swing.undo.AbstractUndoableEdit;

import net.sf.jabref.Globals;


public class UndoableMoveGroup extends AbstractUndoableEdit {
    private final GroupSelector m_groupSelector;
    private final GroupTreeNode m_groupsRootHandle;
    private final int[] m_pathToNewParent;
    private final int m_newChildIndex;
    private final int[] m_pathToOldParent;
    private final int m_oldChildIndex;

    
    public UndoableMoveGroup(GroupSelector gs, GroupTreeNode groupsRoot,
            GroupTreeNode moveNode, GroupTreeNode newParent, int newChildIndex) {
        m_groupSelector = gs;
        m_groupsRootHandle = groupsRoot;
        m_pathToNewParent = newParent.getIndexedPath();
        m_newChildIndex = newChildIndex;
        m_pathToOldParent = ((GroupTreeNode) moveNode.getParent())
                .getIndexedPath();
        m_oldChildIndex = moveNode.getParent().getIndex(moveNode);
    }

    public String getUndoPresentationName() {
        return Globals.lang("Undo") + ": " 
            + Globals.lang("move group");
    }

    public String getRedoPresentationName() {
        return Globals.lang("Redo") + ": " 
            + Globals.lang("move group");
    }

    public void undo() {
        super.undo();
        GroupTreeNode cursor = m_groupsRootHandle
                .getDescendant(m_pathToNewParent);
        cursor = (GroupTreeNode) cursor.getChildAt(m_newChildIndex);
        m_groupsRootHandle.getDescendant(m_pathToOldParent).insert(cursor,
                m_oldChildIndex);
        m_groupSelector.revalidateGroups();
    }

    public void redo() {
        super.redo();
        GroupTreeNode cursor = m_groupsRootHandle
                .getDescendant(m_pathToOldParent);
        cursor = (GroupTreeNode) cursor.getChildAt(m_oldChildIndex);
        m_groupsRootHandle.getDescendant(m_pathToNewParent).insert(cursor,
                m_newChildIndex);
        m_groupSelector.revalidateGroups();
    }
}
