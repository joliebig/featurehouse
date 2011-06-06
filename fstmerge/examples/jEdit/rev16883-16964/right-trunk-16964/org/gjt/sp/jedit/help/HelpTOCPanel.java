

package org.gjt.sp.jedit.help;


import javax.swing.*;
import javax.swing.border.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.jedit.browser.FileCellRenderer; 
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;
import org.gjt.sp.util.XMLUtilities;

import static javax.swing.tree.TreeSelectionModel.SINGLE_TREE_SELECTION;


public class HelpTOCPanel extends JPanel
{
	
	public HelpTOCPanel(HelpViewerInterface helpViewer)
	{
		super(new BorderLayout());

		this.helpViewer = helpViewer;
		nodes = new Hashtable();

		toc = new TOCTree();

		
		if(!OperatingSystem.isMacOSLF())
			toc.putClientProperty("JTree.lineStyle", "Angled");

		toc.setCellRenderer(new TOCCellRenderer());
		toc.setEditable(false);
		toc.setShowsRootHandles(true);

		add(BorderLayout.CENTER,new JScrollPane(toc));

		load();
	} 

	
	public void selectNode(String shortURL)
	{
		if(tocModel == null)
			return;

		DefaultMutableTreeNode node = (DefaultMutableTreeNode)nodes.get(shortURL);

		if(node == null)
			return;

		TreePath path = new TreePath(tocModel.getPathToRoot(node));
		toc.expandPath(path);
		toc.setSelectionPath(path);
		toc.scrollPathToVisible(path);
	} 

	
	public void load()
	{
		DefaultTreeModel empty = new DefaultTreeModel(
			new DefaultMutableTreeNode(
			jEdit.getProperty("helpviewer.toc.loading")));
		toc.setModel(empty);
		toc.setRootVisible(true);

		VFSManager.runInWorkThread(new Runnable()
		{
			public void run()
			{
				createTOC();
				tocModel.reload(tocRoot);
				toc.setModel(tocModel);
				toc.setRootVisible(false);
				for(int i = 0; i <tocRoot.getChildCount(); i++)
				{
					DefaultMutableTreeNode node =
						(DefaultMutableTreeNode)
						tocRoot.getChildAt(i);
					toc.expandPath(new TreePath(
						node.getPath()));
				}
				if(helpViewer.getShortURL() != null)
					selectNode(helpViewer.getShortURL());
			}
		});
	} 

	
	private HelpViewerInterface helpViewer;
	private DefaultTreeModel tocModel;
	private DefaultMutableTreeNode tocRoot;
	private JTree toc;
	private Hashtable nodes;

	
	private DefaultMutableTreeNode createNode(String href, String title)
	{
		DefaultMutableTreeNode node = new DefaultMutableTreeNode(
			new HelpNode(href,title),true);
		nodes.put(href,node);
		return node;
	} 

	
	private void createTOC()
	{
		EditPlugin[] plugins = jEdit.getPlugins();
		Arrays.sort(plugins,new PluginCompare());
		tocRoot = new DefaultMutableTreeNode();

		tocRoot.add(createNode("welcome.html",
			jEdit.getProperty("helpviewer.toc.welcome")));

		tocRoot.add(createNode("README.txt",
			jEdit.getProperty("helpviewer.toc.readme")));
		tocRoot.add(createNode("CHANGES.txt",
			jEdit.getProperty("helpviewer.toc.changes")));
		tocRoot.add(createNode("TODO.txt",
			jEdit.getProperty("helpviewer.toc.todo")));
		tocRoot.add(createNode("COPYING.txt",
			jEdit.getProperty("helpviewer.toc.copying")));
		tocRoot.add(createNode("COPYING.DOC.txt",
			jEdit.getProperty("helpviewer.toc.copying-doc")));
		tocRoot.add(createNode("Apache.LICENSE.txt",
			jEdit.getProperty("helpviewer.toc.copying-apache")));
		tocRoot.add(createNode("COPYING.PLUGINS.txt",
			jEdit.getProperty("helpviewer.toc.copying-plugins")));

		loadTOC(tocRoot,"news44/toc.xml");
		loadTOC(tocRoot,"users-guide/toc.xml");
		loadTOC(tocRoot,"FAQ/toc.xml");


		DefaultMutableTreeNode pluginTree = new DefaultMutableTreeNode(
			jEdit.getProperty("helpviewer.toc.plugins"),true);

		for(int i = 0; i < plugins.length; i++)
		{
			EditPlugin plugin = plugins[i];

			String name = plugin.getClassName();

			String docs = jEdit.getProperty("plugin." + name + ".docs");
			String label = jEdit.getProperty("plugin." + name + ".name");
			if(docs != null)
			{
				if(label != null && docs != null)
				{
					String path = plugin.getPluginJAR()
						.getClassLoader()
						.getResourceAsPath(docs);
					pluginTree.add(createNode(
						path,label));
				}
			}
		}

		if(pluginTree.getChildCount() != 0)
			tocRoot.add(pluginTree);
		else
		{
			
			pluginTree = null;
		}
		loadTOC(tocRoot,"api/toc.xml");
		tocModel = new DefaultTreeModel(tocRoot);
	} 

	
	private void loadTOC(DefaultMutableTreeNode root, String path)
	{
		TOCHandler h = new TOCHandler(root,MiscUtilities.getParentOfPath(path));
		try
		{
			XMLUtilities.parseXML(
				new URL(helpViewer.getBaseURL()
					+ '/' + path).openStream(), h);
		}
		catch(IOException e)
		{
			Log.log(Log.ERROR,this,e);
		}
	} 

	

	
	static class HelpNode
	{
		String href, title;

		
		HelpNode(String href, String title)
		{
			this.href = href;
			this.title = title;
		} 

		
		public String toString()
		{
			return title;
		} 
	} 

	
	class TOCHandler extends DefaultHandler
	{
		String dir;

		
		TOCHandler(DefaultMutableTreeNode root, String dir)
		{
			nodes = new Stack();
			node = root;
			this.dir = dir;
		} 

		
		public void characters(char[] c, int off, int len)
		{
			if(tag.equals("TITLE"))
			{
				boolean firstNonWhitespace = false;
				for(int i = 0; i < len; i++)
				{
					char ch = c[off + i];
					if (!firstNonWhitespace && Character.isWhitespace(ch)) continue;
					firstNonWhitespace = true;
					title.append(ch);
				}
			}


		} 

		
		public void startElement(String uri, String localName,
					 String name, Attributes attrs)
		{
			tag = name;
			if (name.equals("ENTRY"))
				href = attrs.getValue("HREF");
		} 

		
		public void endElement(String uri, String localName, String name)
		{
			if(name == null)
				return;

			if(name.equals("TITLE"))
			{
				DefaultMutableTreeNode newNode = createNode(
					dir + href,title.toString());
				node.add(newNode);
				nodes.push(node);
				node = newNode;
				title.setLength(0);
			}
			else if(name.equals("ENTRY"))
			{
				node = (DefaultMutableTreeNode)nodes.pop();
				href = null;
			}
		} 

		
		private String tag;
		private StringBuilder title = new StringBuilder();
		private String href;
		private DefaultMutableTreeNode node;
		private Stack nodes;
		
	} 

	
	class TOCTree extends JTree
	{
		
		TOCTree()
		{
			ToolTipManager.sharedInstance().registerComponent(this);
			selectionModel.setSelectionMode(SINGLE_TREE_SELECTION);
		} 

		
		public final String getToolTipText(MouseEvent evt)
		{
			TreePath path = getPathForLocation(evt.getX(), evt.getY());
			if(path != null)
			{
				Rectangle cellRect = getPathBounds(path);
				if(cellRect != null && !cellRectIsVisible(cellRect))
					return path.getLastPathComponent().toString();
			}
			return null;
		} 

		
		 

		
		public void processKeyEvent(KeyEvent evt)
		{
			if ((KeyEvent.KEY_PRESSED == evt.getID()) &&
			    (KeyEvent.VK_ENTER == evt.getKeyCode()))
			{
				TreePath path = getSelectionPath();
				if(path != null)
				{
					Object obj = ((DefaultMutableTreeNode)
						path.getLastPathComponent())
						.getUserObject();
					if(!(obj instanceof HelpNode))
					{
						this.expandPath(path);
						return;
					}

					HelpNode node = (HelpNode)obj;
					helpViewer.gotoURL(node.href,true,0);
				}
				evt.consume();
			}
			else
			{
				super.processKeyEvent(evt);
			}
		} 

		
		protected void processMouseEvent(MouseEvent evt)
		{
			

			switch(evt.getID())
			{
			
			case MouseEvent.MOUSE_CLICKED:
				TreePath path = getPathForLocation(evt.getX(),evt.getY());
				if(path != null)
				{
					if(!isPathSelected(path))
						setSelectionPath(path);

					Object obj = ((DefaultMutableTreeNode)
						path.getLastPathComponent())
						.getUserObject();
					if(!(obj instanceof HelpNode))
					{
						this.expandPath(path);
						return;
					}

					HelpNode node = (HelpNode)obj;

					helpViewer.gotoURL(node.href,true,0);
				}

				super.processMouseEvent(evt);
				break;
			default:
				super.processMouseEvent(evt);
				break;
			}
		} 

		
		private boolean cellRectIsVisible(Rectangle cellRect)
		{
			Rectangle vr = TOCTree.this.getVisibleRect();
			return vr.contains(cellRect.x,cellRect.y) &&
				vr.contains(cellRect.x + cellRect.width,
				cellRect.y + cellRect.height);
		} 
	} 

	
	static class TOCCellRenderer extends DefaultTreeCellRenderer
	{
		EmptyBorder border = new EmptyBorder(1,0,1,1);

		public Component getTreeCellRendererComponent(JTree tree,
			Object value, boolean sel, boolean expanded,
			boolean leaf, int row, boolean focus)
		{
			super.getTreeCellRendererComponent(tree,value,sel,
				expanded,leaf,row,focus);
			setIcon(leaf ? FileCellRenderer.fileIcon
				: (expanded ? FileCellRenderer.openDirIcon
				: FileCellRenderer.dirIcon));
			setBorder(border);

			return this;
		}
	} 

	
	static class PluginCompare implements Comparator
	{
		public int compare(Object o1, Object o2)
		{
			EditPlugin p1 = (EditPlugin)o1;
			EditPlugin p2 = (EditPlugin)o2;
			return StandardUtilities.compareStrings(
				jEdit.getProperty("plugin." + p1.getClassName() + ".name"),
				jEdit.getProperty("plugin." + p2.getClassName() + ".name"),
				true);
		}
	} 
}