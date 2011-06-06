

package org.gjt.sp.jedit.search;


import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;



public class HyperSearchOperationNode
{
	private boolean treeViewDisplayed;
	private final String searchString;
	private List<DefaultMutableTreeNode> resultNodes;
	private SearchMatcher searchMatcher;
	
	
	public HyperSearchOperationNode(String searchString, SearchMatcher searchMatcher)
	{
		this.searchString = searchString;
		this.searchMatcher = searchMatcher;
	}
	
	
	public String toString() 
	{
		return searchString;
	}
	
	
	public boolean isTreeViewDisplayed() 
	{
		return treeViewDisplayed;
	}
	
	
	public void setTreeViewDisplayed(boolean treeViewDisplayed) 
	{
		this.treeViewDisplayed = treeViewDisplayed;
	}
	
	
	public void restoreFlatNodes(JTree resultTree, DefaultMutableTreeNode operNode)
	{
		for (int i = 0; i < resultNodes.size(); i++)
		{
			DefaultMutableTreeNode element = resultNodes.get(i);
			if (element.getUserObject() instanceof HyperSearchFileNode)
				((HyperSearchFileNode)element.getUserObject()).showFullPath = true;

			operNode.insert(element, operNode.getChildCount());
		}

		((DefaultTreeModel)resultTree.getModel()).nodeStructureChanged(operNode);
		
		for (Enumeration e = operNode.children(); e.hasMoreElements();)
		{
			DefaultMutableTreeNode node = (DefaultMutableTreeNode)e.nextElement();
			resultTree.expandPath(new TreePath(node.getPath()));
		}
		resultTree.scrollPathToVisible(
			new TreePath(operNode.getPath()));
	}
	
	
	public void cacheResultNodes(DefaultMutableTreeNode operNode) 
	{
		resultNodes = new ArrayList<DefaultMutableTreeNode>(operNode.getChildCount());
		for (Enumeration e = operNode.children(); e.hasMoreElements();)
			resultNodes.add((DefaultMutableTreeNode) e.nextElement());
	}
	
	
	public static void removeNodeFromCache(MutableTreeNode mnode)
	{
		DefaultMutableTreeNode node = (DefaultMutableTreeNode)mnode;
		if (node.getUserObject() instanceof HyperSearchOperationNode)
			return;
		
		DefaultMutableTreeNode tmpNode = node;
		while ((tmpNode = (DefaultMutableTreeNode) tmpNode.getParent()) != null)
		{
			if (!(tmpNode.getUserObject() instanceof HyperSearchOperationNode))
				continue;
			HyperSearchOperationNode operNode = (HyperSearchOperationNode) tmpNode.getUserObject();
			if (operNode.resultNodes != null)
			{
				
				operNode.resultNodes.remove(node);
			}
			break;
		}
		
	}
	
	
	public void insertTreeNodes(JTree resultTree, DefaultMutableTreeNode operNode)
	{
		String fileSep = System.getProperty("file.separator");
		String fileSepRegex = System.getProperty("file.separator");
		if (fileSep.equals("\\"))
			fileSepRegex = "\\\\";
		
		
		String[] topPathTmp = null;
		int topPathNdx = -1;

		for (int i = 0;i < resultNodes.size();i++)
		{
			DefaultMutableTreeNode fileTreeNode = resultNodes.get(i);
			Object obj = fileTreeNode.getUserObject();
			if (!(obj instanceof HyperSearchFileNode))
				continue;
			HyperSearchFileNode fileNode = (HyperSearchFileNode)obj;

			int pos = fileNode.path.lastIndexOf(fileSep);
			String pathName = fileNode.path.substring(0, pos);
			String[] paths = pathName.split(fileSepRegex);
			if (topPathNdx == -1)
			{
				topPathNdx = paths.length;
				topPathTmp = paths;
			}
			else if (paths.length < topPathNdx)
			{
				topPathNdx = paths.length;
				topPathTmp = paths;				
			}
			else
			{
				for (int ndx =0 ; ndx < topPathNdx; ndx++)
				{
					if (!paths[ndx].equals(topPathTmp[ndx]))
					{
						topPathNdx = ndx;
						break;
					}
				}
			}
		}
		String[] topPath = new String[topPathNdx];
		String topPathPath = "";
		for (int ndx = 0 ; ndx < topPathNdx; ndx++)
		{
			topPath[ndx] = topPathTmp[ndx];
			topPathPath = topPathPath.concat(topPath[ndx] + fileSep);
		}
		Map<String, DefaultMutableTreeNode> treeNodes = new HashMap<String, DefaultMutableTreeNode>();
		HyperSearchFolderNode folderNode = 
			new HyperSearchFolderNode(new File(topPathPath), true);
		DefaultMutableTreeNode folderTreeNode = new DefaultMutableTreeNode(folderNode);
		operNode.insert(folderTreeNode, operNode.getChildCount());
		treeNodes.put(topPathPath, folderTreeNode);
		
		for (int i = 0;i < resultNodes.size();i++)
		{
			DefaultMutableTreeNode fileTreeNode = resultNodes.get(i);
			Object obj = fileTreeNode.getUserObject();
			if (!(obj instanceof HyperSearchFileNode))
				continue;
			HyperSearchFileNode fileNode = (HyperSearchFileNode)obj;

			fileNode.showFullPath = false;
			int pos = fileNode.path.lastIndexOf(fileSep);
			String pathName = fileNode.path.substring(0, pos);
			String[] paths = pathName.split(fileSepRegex);
			
			DefaultMutableTreeNode insNode = folderTreeNode;
			String partialPath = topPathPath;
			for (int ndx = topPathNdx; ndx < paths.length; ndx++)
			{
				partialPath = partialPath.concat(paths[ndx] + fileSep);
				DefaultMutableTreeNode tmpNode = treeNodes.get(partialPath);
				if (tmpNode == null)
				{
					HyperSearchFolderNode tmpFolderNode = 
						new HyperSearchFolderNode(new File(partialPath), false);
					tmpNode = new DefaultMutableTreeNode(tmpFolderNode);
					insNode.insert(tmpNode, insNode.getChildCount());
					treeNodes.put(partialPath, tmpNode);
				}
				insNode = tmpNode;
			}
			insNode.insert(fileTreeNode, insNode.getChildCount());
			treeNodes.put(fileNode.path, insNode);
		}
		
	}

	
	public SearchMatcher getSearchMatcher()
	{
		return searchMatcher;
	}

	
	public String getSearchString()
	{
		return searchString;
	}

}
