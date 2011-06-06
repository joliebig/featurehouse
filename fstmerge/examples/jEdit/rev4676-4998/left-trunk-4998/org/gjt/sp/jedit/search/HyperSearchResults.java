

package org.gjt.sp.jedit.search;


import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.*;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.gui.DefaultFocusComponent;
import org.gjt.sp.jedit.gui.RolloverButton;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.jedit.*;



public class HyperSearchResults extends JPanel implements EBComponent,
	DefaultFocusComponent
{
	public static final String NAME = "hypersearch-results";

	
	public HyperSearchResults(View view)
	{
		super(new BorderLayout());

		this.view = view;

		caption = new JLabel();

		Box toolBar = new Box(BoxLayout.X_AXIS);
		toolBar.add(caption);
		toolBar.add(Box.createGlue());

		ActionHandler ah = new ActionHandler();

		clear = new RolloverButton(GUIUtilities.loadIcon("Clear.png"));
		clear.setToolTipText(jEdit.getProperty(
			"hypersearch-results.clear.label"));
		clear.addActionListener(ah);
		toolBar.add(clear);

		multi = new RolloverButton();
		multi.setToolTipText(jEdit.getProperty(
			"hypersearch-results.multi.label"));
		multi.addActionListener(ah);
		toolBar.add(multi);

		add(BorderLayout.NORTH, toolBar);

		resultTreeRoot = new DefaultMutableTreeNode();
		resultTreeModel = new DefaultTreeModel(resultTreeRoot);
		resultTree = new JTree(resultTreeModel);
		resultTree.setCellRenderer(new ResultCellRenderer());
		resultTree.setVisibleRowCount(16);
		resultTree.setRootVisible(false);
		resultTree.setShowsRootHandles(true);

		
		if(!OperatingSystem.isMacOSLF())
			resultTree.putClientProperty("JTree.lineStyle", "Angled");

		resultTree.setEditable(false);

		resultTree.addKeyListener(new KeyHandler());
		resultTree.addMouseListener(new MouseHandler());

		JScrollPane scrollPane = new JScrollPane(resultTree);
		Dimension dim = scrollPane.getPreferredSize();
		dim.width = 400;
		scrollPane.setPreferredSize(dim);
		add(BorderLayout.CENTER, scrollPane);
	} 

	
	public void focusOnDefaultComponent()
	{
		resultTree.requestFocus();
	} 

	
	public void addNotify()
	{
		super.addNotify();
		EditBus.addToBus(this);
		multiStatus = jEdit.getBooleanProperty(
			"hypersearch-results.multi");
		updateMultiStatus();
	} 

	
	public void removeNotify()
	{
		super.removeNotify();
		EditBus.removeFromBus(this);
		jEdit.setBooleanProperty("hypersearch-results.multi",multiStatus);
	} 

	
	public void handleMessage(EBMessage msg)
	{
		if(msg instanceof BufferUpdate)
		{
			BufferUpdate bmsg = (BufferUpdate)msg;
			Buffer buffer = bmsg.getBuffer();
			Object what = bmsg.getWhat();
			if(what == BufferUpdate.LOADED ||
				what == BufferUpdate.CLOSED)
			{
				ResultVisitor visitor = null;
				if (what == BufferUpdate.LOADED)
				{
					visitor = new BufferLoadedVisitor();
				}
				else 
				{
					visitor = new BufferClosedVisitor();
				}
				
				
				for(int i = resultTreeRoot.getChildCount() - 1; i >= 0; i--)
				{
					DefaultMutableTreeNode searchNode = (DefaultMutableTreeNode)
						resultTreeRoot.getChildAt(i);
					for(int j = searchNode.getChildCount() - 1;
						j >= 0; j--)
					{

						DefaultMutableTreeNode bufferNode = (DefaultMutableTreeNode)
							searchNode.getChildAt(j);

						for(int k = bufferNode.getChildCount() - 1;
							k >= 0; k--)
						{
							Object userObject =
								((DefaultMutableTreeNode)bufferNode
								.getChildAt(k)).getUserObject();
							HyperSearchResult result = (HyperSearchResult)
									userObject;

							if(buffer.getPath().equals(result.path))
								visitor.visit(buffer,result);
						}
					}
				}
			}
		}
	} 

	
	public DefaultTreeModel getTreeModel()
	{
		return resultTreeModel;
	} 

	
	
	public JTree getTree()
	{
		return resultTree;
	} 

	
	public void searchStarted()
	{
		caption.setText(jEdit.getProperty("hypersearch-results.searching"));
	} 

	
	public void searchFailed()
	{
		caption.setText(jEdit.getProperty("hypersearch-results.no-results"));

		
		for(int i = 0; i < resultTreeRoot.getChildCount(); i++)
		{
			DefaultMutableTreeNode node = (DefaultMutableTreeNode)
				resultTreeRoot.getChildAt(i);
			resultTree.collapsePath(new TreePath(new Object[] {
				resultTreeRoot, node }));
		}
	} 

	
	public void searchDone(final DefaultMutableTreeNode searchNode)
	{
		final int nodeCount = searchNode.getChildCount();
		if (nodeCount < 1)
		{
			searchFailed();
			return;
		}

		caption.setText(jEdit.getProperty("hypersearch-results.done"));

		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				if(!multiStatus)
				{
					for(int i = 0; i < resultTreeRoot.getChildCount(); i++)
					{
						resultTreeRoot.remove(0);
					}
				}

				resultTreeRoot.add(searchNode);
				resultTreeModel.reload(resultTreeRoot);

				TreePath lastNode = null;

				for(int i = 0; i < nodeCount; i++)
				{
					lastNode = new TreePath(
						((DefaultMutableTreeNode)
						searchNode.getChildAt(i))
						.getPath());

					resultTree.expandPath(lastNode);
				}

				resultTree.scrollPathToVisible(
					new TreePath(new Object[] {
					resultTreeRoot,searchNode }));
			}
		});
	} 

	
	private View view;

	private JLabel caption;
	private JTree resultTree;
	private DefaultMutableTreeNode resultTreeRoot;
	private DefaultTreeModel resultTreeModel;

	private RolloverButton clear;
	private RolloverButton multi;
	private boolean multiStatus;

	
	private void updateMultiStatus()
	{
		if(multiStatus)
			multi.setIcon(GUIUtilities.loadIcon("MultipleResults.png"));
		else
			multi.setIcon(GUIUtilities.loadIcon("SingleResult.png"));
	} 

	
	private void goToSelectedNode()
	{
		TreePath path = resultTree.getSelectionPath();
		if(path == null)
			return;

		DefaultMutableTreeNode node = (DefaultMutableTreeNode)path
			.getLastPathComponent();
		Object value = node.getUserObject();

		if(node.getParent() == resultTreeRoot)
		{
			
		}
		else if(value instanceof String)
		{
			Buffer buffer = jEdit.openFile(view,(String)value);
			if(buffer == null)
				return;

			view.goToBuffer(buffer);

			
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					resultTree.requestFocus();
				}
			});
		}
		else if (value instanceof HyperSearchResult)
		{
			((HyperSearchResult)value).goTo(view);
		}
	} 

	

	
	public class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == clear)
			{
				resultTreeRoot.removeAllChildren();
				resultTreeModel.reload(resultTreeRoot);
			}
			else if(source == multi)
			{
				multiStatus = !multiStatus;
				updateMultiStatus();

				if(!multiStatus)
				{
					for(int i = resultTreeRoot.getChildCount() - 2; i >= 0; i--)
					{
						resultTreeModel.removeNodeFromParent(
							(MutableTreeNode)resultTreeRoot
							.getChildAt(i));
					}
				}
			}
		}
	} 

	
	class KeyHandler extends KeyAdapter
	{
		public void keyPressed(KeyEvent evt)
		{
			if(evt.getKeyCode() == KeyEvent.VK_ENTER)
			{
				goToSelectedNode();

				
				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						resultTree.requestFocus();
					}
				});

				evt.consume();
			}
		}
	} 

	
	class MouseHandler extends MouseAdapter
	{
		
		public void mousePressed(MouseEvent evt)
		{
			if(evt.isConsumed())
				return;

			TreePath path1 = resultTree.getPathForLocation(
				evt.getX(),evt.getY());
			if(path1 == null)
				return;

			resultTree.setSelectionPath(path1);
			if (GUIUtilities.isPopupTrigger(evt))
				showPopupMenu(evt);
			else
			{
				goToSelectedNode();

				view.toFront();
				view.requestFocus();
				view.getTextArea().requestFocus();
			}
		} 

		
		private JPopupMenu popupMenu;

		
		private void showPopupMenu(MouseEvent evt)
		{
			if (popupMenu == null)
			{
				popupMenu = new JPopupMenu();
				popupMenu.add(new RemoveTreeNodeAction());
			}

			GUIUtilities.showPopupMenu(popupMenu,evt.getComponent(),
				evt.getX(),evt.getY());
			evt.consume();
		} 

		
	} 

	
	class RemoveTreeNodeAction extends AbstractAction
	{
		public RemoveTreeNodeAction()
		{
			super(jEdit.getProperty("hypersearch-results.remove-node"));
		}

		public void actionPerformed(ActionEvent evt)
		{
			TreePath path = resultTree.getSelectionPath();
			if(path == null)
				return;

			MutableTreeNode value = (MutableTreeNode)path
				.getLastPathComponent();
			resultTreeModel.removeNodeFromParent(value);
		}
	}

	
	class RemoveAllTreeNodesAction extends AbstractAction
	{
		public RemoveAllTreeNodesAction()
		{
			super(jEdit.getProperty("hypersearch-results.remove-all-nodes"));
		}

		public void actionPerformed(ActionEvent evt)
		{
			resultTreeRoot = new DefaultMutableTreeNode();
			resultTreeModel = new DefaultTreeModel(resultTreeRoot);
			resultTree.setModel(resultTreeModel);
		}
	}

	
	class ResultCellRenderer extends DefaultTreeCellRenderer
	{
		Font plainFont, boldFont;

		
		ResultCellRenderer()
		{
			plainFont = UIManager.getFont("Tree.font");
			if(plainFont == null)
				plainFont = jEdit.getFontProperty("metal.secondary.font");
			boldFont = new Font(plainFont.getName(),Font.BOLD,
				plainFont.getSize());
		} 

		
		public Component getTreeCellRendererComponent(JTree tree,
			Object value, boolean sel, boolean expanded,
			boolean leaf, int row, boolean hasFocus)
		{
			Component comp = super.getTreeCellRendererComponent(tree,value,sel,
				expanded,leaf,row,hasFocus);
			setIcon(null);
			DefaultMutableTreeNode node = (DefaultMutableTreeNode)value;

			if (node.getParent() == resultTreeRoot)
			{
				ResultCellRenderer.this.setFont(boldFont);
				int bufferCount = node.getChildCount();
				int resultCount = 0;
				for (int i = 0; i < bufferCount; i++)
				{
					resultCount += node.getChildAt(i).getChildCount();
				}
				Object[] pp = { node.toString(), new Integer(resultCount), new Integer(bufferCount) };
				setText(jEdit.getProperty("hypersearch-results.result-caption",pp));
			}
			else if(node.getUserObject() instanceof String)
			{
				
				ResultCellRenderer.this.setFont(boldFont);
				int count = node.getChildCount();
				if(count == 1)
				{
					setText(jEdit.getProperty("hypersearch-results"
						+ ".file-caption1",new Object[] {
						node.getUserObject()
						}));
				}
				else
				{
					setText(jEdit.getProperty("hypersearch-results"
						+ ".file-caption",new Object[] {
						node.getUserObject(),
						new Integer(count)
						}));
				}
			}
			else
			{
				ResultCellRenderer.this.setFont(plainFont);
			}

			return this;
		} 
	} 

	
	
	

	
	interface ResultVisitor
	{
		public void visit(Buffer buffer, HyperSearchResult result);
	} 

	
	class BufferLoadedVisitor implements ResultVisitor
	{
		public void visit(Buffer buffer, HyperSearchResult result)
		{
			result.bufferOpened(buffer);
		}
	} 

	
	class BufferClosedVisitor implements ResultVisitor
	{
		public void visit(Buffer buffer, HyperSearchResult result)
		{
			result.bufferClosed();
		}
	} 
}
