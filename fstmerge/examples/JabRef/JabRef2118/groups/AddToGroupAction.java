

package net.sf.jabref.groups; 

import java.awt.event.ActionEvent; 
import java.util.*; 

import javax.swing.*; 
import javax.swing.undo.AbstractUndoableEdit; 

import net.sf.jabref.*; 
import net.sf.jabref.undo.NamedCompound; 
import java.util.Enumeration; 
import java.util.Vector; 

import javax.swing.AbstractAction; 

import net.sf.jabref.BasePanel; 
import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.Globals; 
import net.sf.jabref.Util; 

public  class  AddToGroupAction  extends AbstractAction {
	
    protected GroupTreeNode m_node;

	
    protected final boolean m_move;

	
    protected BasePanel m_panel;

	
    
    public AddToGroupAction(GroupTreeNode node, boolean move,
            BasePanel panel) {
        super(node.getGroup().getName());
        m_node = node;
        m_move = move;
        m_panel = panel;
    }


	
    
    public AddToGroupAction(boolean move) {
        super(Globals.lang(move ? "Assign entry selection exclusively to this group"
                : "Add entry selection to this group")); 
        m_move = move;
    }


	
    public void setBasePanel(BasePanel panel) {
        m_panel = panel;
    }


	
    public void setNode(GroupTreeNode node) {
        m_node = node;
    }


	
    public void actionPerformed(ActionEvent evt) {
        final BibtexEntry[] entries = m_panel.getSelectedEntries();
        final Vector<GroupTreeNode> removeGroupsNodes = new Vector<GroupTreeNode>(); 
        
        if (m_move) {
            
            Enumeration<GroupTreeNode> e = ((GroupTreeNode) m_node.getRoot()).preorderEnumeration();
            GroupTreeNode node;
            while (e.hasMoreElements()) {
                node = (GroupTreeNode) e.nextElement();
                if (!node.getGroup().supportsRemove())
                    continue;
                for (int i = 0; i < entries.length; ++i) {
                    if (node.getGroup().contains(entries[i]))
                        removeGroupsNodes.add(node);
                }
            }
            
            
            AbstractGroup[] groups = new AbstractGroup[removeGroupsNodes.size()+1];
            for (int i = 0; i < removeGroupsNodes.size(); ++i)
                groups[i] = removeGroupsNodes.elementAt(i).getGroup();
            groups[groups.length-1] = m_node.getGroup();
            if (!Util.warnAssignmentSideEffects(groups,
                    entries, m_panel.getDatabase(), m_panel.frame()))
                return; 
        } else {
            
            if (!Util.warnAssignmentSideEffects(new AbstractGroup[]{m_node.getGroup()},
                    entries, m_panel.getDatabase(), m_panel.frame()))
                return; 
        }
        
        
        
        
        m_panel.storeCurrentEdit();
        
        NamedCompound undoAll = new NamedCompound(Globals.lang("change assignment of entries")); 
        
        if (m_move) {
            
            for (int i = 0; i < removeGroupsNodes.size(); ++i) {
                GroupTreeNode node = removeGroupsNodes.elementAt(i);
                if (node.getGroup().containsAny(entries))
                    undoAll.addEdit(node.removeFromGroup(entries));
            }
            
            AbstractUndoableEdit undoAdd = m_node.addToGroup(entries);
            if (undoAdd != null)
                undoAll.addEdit(undoAdd);
        } else {
            AbstractUndoableEdit undoAdd = m_node.addToGroup(entries);
            if (undoAdd == null)
                return; 
            undoAll.addEdit(undoAdd);
        }
        
        undoAll.end();
        
        m_panel.undoManager.addEdit(undoAll);
        m_panel.markBaseChanged();
        m_panel.updateEntryEditorIfShowing();
        m_panel.getGroupSelector().valueChanged(null);
    }



}
