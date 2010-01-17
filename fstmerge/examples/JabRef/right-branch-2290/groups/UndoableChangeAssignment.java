

package net.sf.jabref.groups;

import java.util.*;

import javax.swing.undo.AbstractUndoableEdit;

import net.sf.jabref.*;


public class UndoableChangeAssignment extends AbstractUndoableEdit {
    private final Set m_previousAssignmentBackup;
    private final Set m_newAssignmentBackup;
    
    private int[] m_pathToNode = null;
    
    private GroupTreeNode m_groupsRootHandle = null;

    
    public UndoableChangeAssignment(Set previousAssignment,
            Set currentAssignment) {
        m_previousAssignmentBackup = new HashSet(previousAssignment);
        m_newAssignmentBackup = new HashSet(currentAssignment);
    }

    public UndoableChangeAssignment(Set previousAssignment,
            Set currentAssignment, GroupTreeNode node) {
        this(previousAssignment, currentAssignment);
        setEditedNode(node);
    }

    
    public void setEditedNode(GroupTreeNode node) {
        m_groupsRootHandle = (GroupTreeNode) node.getRoot();
        m_pathToNode = node.getIndexedPath();
    }

    public String getUndoPresentationName() {
        return Globals.lang("Undo") + ": "
                + Globals.lang("change assignment of entries");
    }

    public String getRedoPresentationName() {
        return Globals.lang("Redo") + ": "
                + Globals.lang("change assignment of entries");
    }

    public void undo() {
        super.undo();
        ExplicitGroup group = (ExplicitGroup) m_groupsRootHandle.getChildAt(
                m_pathToNode).getGroup();
        group.clearAssignments();
        for (Iterator it = m_previousAssignmentBackup.iterator(); it.hasNext();)
            group.addEntry((BibtexEntry) it.next());
    }

    public void redo() {
        super.redo();
        ExplicitGroup group = (ExplicitGroup) m_groupsRootHandle.getChildAt(
                m_pathToNode).getGroup();
        group.clearAssignments();
        for (Iterator it = m_newAssignmentBackup.iterator(); it.hasNext();)
            group.addEntry((BibtexEntry) it.next());
    }
}
