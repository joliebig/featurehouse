
package net.sf.jabref.groups;

import java.util.Vector;

import javax.swing.tree.TreeNode;
import javax.swing.undo.AbstractUndoableEdit;

import net.sf.jabref.Globals;

public class UndoableModifySubtree extends AbstractUndoableEdit {
    
    private final GroupTreeNode m_subtreeBackup;
    
    private final int[] m_subtreeRootPath;
    private final GroupSelector m_groupSelector;
    
    private Vector<TreeNode> m_modifiedSubtree = new Vector<TreeNode>();
    private boolean m_revalidate = true;
    private final String m_name;

    
    public UndoableModifySubtree(GroupSelector groupSelector,
            GroupTreeNode subtree, String name) {
        m_subtreeBackup = subtree.deepCopy();
        m_subtreeRootPath = subtree.getIndexedPath();
        m_groupSelector = groupSelector;
        m_name = name;
    }

    public String getUndoPresentationName() {
        return Globals.lang("Undo") + ": " + m_name;
        
    }

    public String getRedoPresentationName() {
        return Globals.lang("Redo") + ": " + m_name;
    }

    public void undo() {
        super.undo();
        
        m_modifiedSubtree.clear();
        
        final GroupTreeNode subtreeRoot = m_groupSelector.getGroupTreeRoot()
                .getNode(m_subtreeRootPath);
        for (int i = 0; i < subtreeRoot.getChildCount(); ++i)
            m_modifiedSubtree.add(subtreeRoot.getChildAt(i));
        
        subtreeRoot.removeAllChildren();
        for (int i = 0; i < m_subtreeBackup.getChildCount(); ++i)
            subtreeRoot.add(((GroupTreeNode) m_subtreeBackup.getChildAt(i))
                    .deepCopy());
        if (m_revalidate)
            m_groupSelector.revalidateGroups();
    }

    public void redo() {
        super.redo();
        final GroupTreeNode subtreeRoot = m_groupSelector.getGroupTreeRoot()
                .getNode(m_subtreeRootPath);
        subtreeRoot.removeAllChildren();
        for (int i = 0; i < m_modifiedSubtree.size(); ++i)
            subtreeRoot.add((GroupTreeNode) m_modifiedSubtree.elementAt(i));
        if (m_revalidate)
            m_groupSelector.revalidateGroups();
    }

    
    public void setRevalidate(boolean revalidate) {
        m_revalidate = revalidate;
    }
}
