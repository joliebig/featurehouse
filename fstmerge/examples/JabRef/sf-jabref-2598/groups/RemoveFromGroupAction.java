

package net.sf.jabref.groups;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.undo.AbstractUndoableEdit;

import net.sf.jabref.BasePanel;
import net.sf.jabref.Globals;
import net.sf.jabref.Util;

public class RemoveFromGroupAction extends AbstractAction {
    protected GroupTreeNode m_node;
    protected BasePanel m_panel;
    public RemoveFromGroupAction(GroupTreeNode node, BasePanel panel) {
        super(node.getGroup().getName());
        m_node = node;
        m_panel = panel;
    }
    public RemoveFromGroupAction() {
        super(Globals.lang("Remove entry selection from this group")); 
    }
    public void setNode(GroupTreeNode node) {
        m_node = node;
    }
    public void setBasePanel(BasePanel panel) {
        m_panel = panel;
    }
    public void actionPerformed(ActionEvent evt) {
        
        if (!Util.warnAssignmentSideEffects(new AbstractGroup[]{m_node.getGroup()},
                m_panel.getSelectedEntries(),
                m_panel.getDatabase(),
                m_panel.frame()))
            return; 
        
        AbstractUndoableEdit undo = m_node.removeFromGroup(m_panel.getSelectedEntries());
        if (undo == null)
            return; 
        
        m_panel.undoManager.addEdit(undo);
        m_panel.markBaseChanged();
        m_panel.updateEntryEditorIfShowing();
        m_panel.getGroupSelector().valueChanged(null);
    }
}
