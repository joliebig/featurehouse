

package net.sf.jabref.groups; 

import java.awt.Cursor; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.awt.datatransfer.Transferable; 
import java.awt.datatransfer.UnsupportedFlavorException; 
import java.awt.dnd.*; 
import java.awt.event.InputEvent; 
import java.io.IOException; 
import java.util.Enumeration; 
import java.util.Vector; 

import javax.swing.JTree; 
import javax.swing.SwingUtilities; 
import javax.swing.ToolTipManager; 
import javax.swing.tree.DefaultMutableTreeNode; 
import javax.swing.tree.TreePath; 
import javax.swing.tree.TreeSelectionModel; 
import javax.swing.undo.AbstractUndoableEdit; 

import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.Globals; 
import net.sf.jabref.Util; 

public  class  GroupsTree  extends JTree implements  DragSourceListener , 
		DropTargetListener ,  DragGestureListener {
	
	
	private static final int dragScrollActivationMargin = 10;

	

	
	private static final int dragScrollDistance = 5;

	

	
	private static long lastDragAutoscroll = 0L;

	

	
	private static final long minAutoscrollInterval = 50L;

	

	
	private Point idlePoint;

	

	
	private long idleStartTime = 0L;

	

	
	private static final int idleMargin = 1;

	

	
	private static final long idleTimeToExpandNode = 1000L;

	

	private GroupSelector groupSelector;

	

	private GroupTreeNode dragNode = null;

	

	private final GroupTreeCellRenderer cellRenderer = new GroupTreeCellRenderer();

	

	public GroupsTree(GroupSelector groupSelector) {
		this.groupSelector = groupSelector;
		DragGestureRecognizer dgr = DragSource.getDefaultDragSource()
				.createDefaultDragGestureRecognizer(this,
						DnDConstants.ACTION_MOVE, this);
		
		dgr.setSourceActions(dgr.getSourceActions() & ~InputEvent.BUTTON3_MASK);
		new DropTarget(this, this);
		setCellRenderer(cellRenderer);
		setFocusable(false);
		setToggleClickCount(0);
		ToolTipManager.sharedInstance().registerComponent(this);
		setShowsRootHandles(false);
		setVisibleRowCount(Globals.prefs.getInt("groupsVisibleRows"));
		getSelectionModel().setSelectionMode(
				TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
	}


	

	public void dragEnter(DragSourceDragEvent dsde) {
		
	}


	

	
	public void dragOver(DragSourceDragEvent dsde) {
		final Point p = dsde.getLocation(); 
		SwingUtilities.convertPointFromScreen(p, this);
		final TreePath path = getPathForLocation(p.x, p.y);
		if (path == null) {
			dsde.getDragSourceContext().setCursor(DragSource.DefaultMoveNoDrop);
			return;
		}
		final GroupTreeNode target = (GroupTreeNode) path
				.getLastPathComponent();
		if (target == null || dragNode.isNodeDescendant(target)
				|| dragNode == target) {
			dsde.getDragSourceContext().setCursor(DragSource.DefaultMoveNoDrop);
			return;
		}
		dsde.getDragSourceContext().setCursor(DragSource.DefaultMoveDrop);
	}


	

	public void dropActionChanged(DragSourceDragEvent dsde) {
		
	}


	

	public void dragDropEnd(DragSourceDropEvent dsde) {
		dragNode = null;
	}


	

	public void dragExit(DragSourceEvent dse) {
		
	}


	

	public void dragEnter(DropTargetDragEvent dtde) {
		
	}


	

	
	public void dragOver(DropTargetDragEvent dtde) {
		final Point cursor = dtde.getLocation();
		final long currentTime = System.currentTimeMillis();
		if (idlePoint == null)
			idlePoint = cursor;

		
		final TreePath path = getPathForLocation(cursor.x, cursor.y);
		final GroupTreeNode target = path == null ? null : (GroupTreeNode) path
				.getLastPathComponent();
		setHighlight1Cell(target);

		
		if (dtde.isDataFlavorSupported(GroupTreeNode.flavor)) {
			
			dtde.acceptDrag(DnDConstants.ACTION_MOVE);
		} else if (dtde
				.isDataFlavorSupported(TransferableEntrySelection.flavorInternal)) {
			
			if (path == null) {
				dtde.rejectDrag();
			} else {
				
				
				
				
				
				if (target.getGroup().supportsAdd()) {
					
					dtde.acceptDrag(DnDConstants.ACTION_LINK);
				} else {
					dtde.rejectDrag();
				}
			}
		} else {
			dtde.rejectDrag();
		}

		
		if (Math.abs(cursor.x - idlePoint.x) < idleMargin
				&& Math.abs(cursor.y - idlePoint.y) < idleMargin) {
			if (currentTime - idleStartTime >= idleTimeToExpandNode) {
				if (path != null) {
					expandPath(path);
				}
			}
		} else {
			idlePoint = cursor;
			idleStartTime = currentTime;
		}

		
		if (currentTime - lastDragAutoscroll < minAutoscrollInterval)
			return;
		final Rectangle r = getVisibleRect();
		final boolean scrollUp = cursor.y - r.y < dragScrollActivationMargin;
		final boolean scrollDown = r.y + r.height - cursor.y < dragScrollActivationMargin;
		final boolean scrollLeft = cursor.x - r.x < dragScrollActivationMargin;
		final boolean scrollRight = r.x + r.width - cursor.x < dragScrollActivationMargin;
		if (scrollUp)
			r.translate(0, -dragScrollDistance);
		else if (scrollDown)
			r.translate(0, +dragScrollDistance);
		if (scrollLeft)
			r.translate(-dragScrollDistance, 0);
		else if (scrollRight)
			r.translate(+dragScrollDistance, 0);
		scrollRectToVisible(r);
		lastDragAutoscroll = currentTime;
	}


	

	public void dropActionChanged(DropTargetDragEvent dtde) {
		
	}


	

	public void drop(DropTargetDropEvent dtde) {
		setHighlight1Cell(null);
		try {
			
			final Transferable transferable = dtde.getTransferable();
			final Point p = dtde.getLocation();
			final TreePath path = getPathForLocation(p.x, p.y);
			if (path == null) {
				dtde.rejectDrop();
				return;
			}
			final GroupTreeNode target = (GroupTreeNode) path
					.getLastPathComponent();
			
			if (transferable.isDataFlavorSupported(GroupTreeNode.flavor)) {
				GroupTreeNode source = (GroupTreeNode) transferable
						.getTransferData(GroupTreeNode.flavor);
				if (source == target) {
					dtde.rejectDrop(); 
					return;
				}
				if (source.isNodeDescendant(target)) {
					dtde.rejectDrop();
					return;
				}
				Enumeration<TreePath> expandedPaths = groupSelector.getExpandedPaths();
				UndoableMoveGroup undo = new UndoableMoveGroup(groupSelector,
						groupSelector.getGroupTreeRoot(), source, target,
						target.getChildCount());
				target.add(source);
				dtde.getDropTargetContext().dropComplete(true);
				
				groupSelector.revalidateGroups(new TreePath[] { new TreePath(
						source.getPath()) }, refreshPaths(expandedPaths));
				groupSelector.concludeMoveGroup(undo, source);
			} else if (transferable
					.isDataFlavorSupported(TransferableEntrySelection.flavorInternal)) {
				final AbstractGroup group = target.getGroup();
				if (!group.supportsAdd()) {
					
					
					dtde.rejectDrop();
					return;
				}
				final TransferableEntrySelection selection = (TransferableEntrySelection) transferable
						.getTransferData(TransferableEntrySelection.flavorInternal);
				final BibtexEntry[] entries = selection.getSelection();
				int assignedEntries = 0;
				for (int i = 0; i < entries.length; ++i) {
					if (!target.getGroup().contains(entries[i]))
						++assignedEntries;
				}

				
				
				if (!Util.warnAssignmentSideEffects(
						new AbstractGroup[] { group },
						selection.getSelection(), groupSelector
								.getActiveBasePanel().getDatabase(),
						groupSelector.frame))
					return; 

				
				
				
				groupSelector.getActiveBasePanel().storeCurrentEdit();

				AbstractUndoableEdit undo = group.add(selection.getSelection());
				if (undo instanceof UndoableChangeAssignment)
					((UndoableChangeAssignment) undo).setEditedNode(target);
				dtde.getDropTargetContext().dropComplete(true);
				groupSelector.revalidateGroups();
				groupSelector.concludeAssignment(undo, target, assignedEntries);
			} else {
				dtde.rejectDrop();
				return;
			}
		} catch (IOException ioe) {
			
		} catch (UnsupportedFlavorException e) {
			
		}
	}


	

	public void dragExit(DropTargetEvent dte) {
		setHighlight1Cell(null);
	}


	

	public void dragGestureRecognized(DragGestureEvent dge) {
		GroupTreeNode selectedNode = getSelectedNode();
		if (selectedNode == null)
			return; 
		Cursor cursor = DragSource.DefaultMoveDrop;
		dragNode = selectedNode;
		dge.getDragSource().startDrag(dge, cursor, selectedNode, this);
	}


	

	
	public GroupTreeNode getSelectedNode() {
		TreePath selectionPath = getSelectionPath();
		return selectionPath != null ? (GroupTreeNode) selectionPath
				.getLastPathComponent() : null;
	}


	

    
    public Enumeration<TreePath> refreshPaths(Enumeration<TreePath> paths) {
        Vector<TreePath> freshPaths = new Vector<TreePath>();
        while (paths.hasMoreElements()) {
            freshPaths.add(new TreePath(
                    ((DefaultMutableTreeNode)paths.nextElement()
                            .getLastPathComponent()).getPath()));
        }
        return freshPaths.elements();
    }


	

    
    public TreePath[] refreshPaths(TreePath[] paths) {
        TreePath[] freshPaths = new TreePath[paths.length];
        for (int i = 0; i < paths.length; ++i) {
            freshPaths[i] = new TreePath(((DefaultMutableTreeNode) paths[i]
                            .getLastPathComponent()).getPath());
        }
        return freshPaths;
    }


	

	
	public void setHighlight1Cell(Object cell) {
		cellRenderer.setHighlight1Cell(cell);
		repaint();
	}


	

	
	public void setHighlight2Cells(Object[] cells) {
		cellRenderer.setHighlight2Cells(cells);
		repaint();
	}


	

	
	public void setHighlight3Cells(Object[] cells) {
		cellRenderer.setHighlight3Cells(cells);
		repaint();
	}


	
    
    
    public void setHighlightBorderCell(GroupTreeNode node) {
        cellRenderer.setHighlightBorderCell(node);
        repaint();
    }


	

	
	public void sort(GroupTreeNode node, boolean recursive) {
		sortWithoutRevalidate(node, recursive);
		groupSelector.revalidateGroups();
	}


	

	
	protected void sortWithoutRevalidate(GroupTreeNode node, boolean recursive) {
		if (node.isLeaf())
			return; 
		GroupTreeNode child1, child2;
		int j = node.getChildCount() - 1;
		int lastModified;
		while (j > 0) {
			lastModified = j + 1;
			j = -1;
			for (int i = 1; i < lastModified; ++i) {
				child1 = (GroupTreeNode) node.getChildAt(i - 1);
				child2 = (GroupTreeNode) node.getChildAt(i);
				if (child2.getGroup().getName().compareToIgnoreCase(
						child1.getGroup().getName()) < 0) {
					node.remove(child1);
					node.insert(child1, i);
					j = i;
				}
			}
		}
		if (recursive) {
			for (int i = 0; i < node.getChildCount(); ++i) {
				sortWithoutRevalidate((GroupTreeNode) node.getChildAt(i), true);
			}
		}
	}


	

	
	public void expandSubtree(GroupTreeNode node) {
		for (Enumeration<GroupTreeNode> e = node.depthFirstEnumeration(); e.hasMoreElements();)
			expandPath(new TreePath(e.nextElement().getPath()));
	}


	

	
	public void collapseSubtree(GroupTreeNode node) {
		for (Enumeration<GroupTreeNode> e = node.depthFirstEnumeration(); e.hasMoreElements();)
			collapsePath(new TreePath((e.nextElement())
					.getPath()));
	}


	

	
	public boolean hasExpandedDescendant(TreePath path) {
		GroupTreeNode node = (GroupTreeNode) path.getLastPathComponent();
		for (Enumeration<GroupTreeNode> e = node.children(); e.hasMoreElements();) {
			GroupTreeNode child = e.nextElement();
			if (child.isLeaf())
				continue; 
			TreePath pathToChild = path.pathByAddingChild(child);
			if (isExpanded(pathToChild) || hasExpandedDescendant(pathToChild))
				return true;
		}
		return false;
	}


	

	
	public boolean hasCollapsedDescendant(TreePath path) {
		GroupTreeNode node = (GroupTreeNode) path.getLastPathComponent();
		for (Enumeration<GroupTreeNode> e = node.children(); e.hasMoreElements();) {
			GroupTreeNode child = e.nextElement();
			if (child.isLeaf())
				continue; 
			TreePath pathToChild = path.pathByAddingChild(child);
			if (isCollapsed(pathToChild) || hasCollapsedDescendant(pathToChild))
				return true;
		}
		return false;
	}



}
