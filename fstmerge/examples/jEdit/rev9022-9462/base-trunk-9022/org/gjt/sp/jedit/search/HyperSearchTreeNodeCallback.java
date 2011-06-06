
package org.gjt.sp.jedit.search;

import javax.swing.tree.DefaultMutableTreeNode;


public interface HyperSearchTreeNodeCallback {
	public boolean processNode(DefaultMutableTreeNode node);
}
