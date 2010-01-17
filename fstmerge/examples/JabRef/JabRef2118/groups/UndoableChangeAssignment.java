

package net.sf.jabref.groups; 

import java.util.*; 

import javax.swing.undo.AbstractUndoableEdit; 

import net.sf.jabref.*; 

import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Set; 

import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.Globals; 


public  class  UndoableChangeAssignment  extends AbstractUndoableEdit {
	
    

	
    

	
    
    private int[] m_pathToNode = null;

	
    
    private GroupTreeNode m_groupsRootHandle = null;

	

    
    public UndoableChangeAssignment(Set<BibtexEntry> previousAssignment,
            Set<BibtexEntry> currentAssignment) {
        m_previousAssignmentBackup = new HashSet<BibtexEntry>(previousAssignment);
        m_newAssignmentBackup = new HashSet<BibtexEntry>(currentAssignment);
    }


	

    
    <<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_70234
public UndoableChangeAssignment(Set<BibtexEntry> previousAssignment,
            Set<BibtexEntry> currentAssignment) {
        m_previousAssignmentBackup = new HashSet<BibtexEntry>(previousAssignment);
        m_newAssignmentBackup = new HashSet<BibtexEntry>(currentAssignment);
=======
public UndoableChangeAssignment(Set previousAssignment,
            Set currentAssignment, GroupTreeNode node) {
        this(previousAssignment, currentAssignment);
        setEditedNode(node);
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_70236
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
        for (Iterator<BibtexEntry> it = m_previousAssignmentBackup.iterator(); it.hasNext();)
            group.addEntry(it.next());
    }


	

    public void redo() {
        super.redo();
        ExplicitGroup group = (ExplicitGroup) m_groupsRootHandle.getChildAt(
                m_pathToNode).getGroup();
        group.clearAssignments();
        for (Iterator<BibtexEntry> it = m_newAssignmentBackup.iterator(); it.hasNext();)
            group.addEntry(it.next());
    }


	
    private final Set<BibtexEntry> m_previousAssignmentBackup;

	
    private final Set<BibtexEntry> m_newAssignmentBackup;


}
