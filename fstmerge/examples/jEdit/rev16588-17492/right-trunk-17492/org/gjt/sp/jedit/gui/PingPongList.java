
package org.gjt.sp.jedit.gui;

import org.gjt.sp.util.Log;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;


public class PingPongList<E> extends JSplitPane
{
	private final MyListModel<E> leftModel;
	private final MyListModel<E> rightModel;
	private JList left;
	private JList right;
	private JLabel leftLabel;
	private JLabel rightLabel;
	private JPanel leftPanel;
	private JPanel rightPanel;

	public PingPongList(List<E> leftData, List<E> rightData)
	{
		this(JSplitPane.HORIZONTAL_SPLIT, leftData, rightData);
	}

	public PingPongList(int newOrientation, List<E> leftData, List<E> rightData)
	{
		super(newOrientation);
		leftModel = new MyListModel<E>(leftData);
		left = new JList(leftModel);
		rightModel = new MyListModel<E>(rightData);
		right = new JList(rightModel);
		leftPanel = new JPanel(new BorderLayout());
		rightPanel = new JPanel(new BorderLayout());
		JScrollPane leftScroll = new JScrollPane(left);
		JScrollPane rightScroll = new JScrollPane(right);
		leftPanel.add(leftScroll);
		rightPanel.add(rightScroll);
		setLeftComponent(leftPanel);
		setRightComponent(rightPanel);
		left.setDragEnabled(true);
		right.setDragEnabled(true);

		MyTransferHandler myTransferHandler = new MyTransferHandler();
		left.setTransferHandler(myTransferHandler);
		right.setTransferHandler(myTransferHandler);
		setDividerLocation(0.5);
	}

	public void setLeftTooltip(String leftTooltip)
	{
		left.setToolTipText(leftTooltip);
	}

	public void setRightTooltip(String rightTooltip)
	{
		right.setToolTipText(rightTooltip);
	}

	public void setLeftTitle(String leftTitle)
	{
		if (leftTitle == null)
		{
			removeLeftTitle();
			return;
		}
		if (leftLabel == null)
		{
			leftLabel = new JLabel();
		}
		leftLabel.setText(leftTitle);
		leftPanel.add(leftLabel, BorderLayout.NORTH);
	}

	public void setRightTitle(String rightTitle)
	{
		if (rightTitle == null)
		{
			removeRightTitle();
			return;
		}
		if (rightLabel == null)
		{
			rightLabel = new JLabel();
		}
		rightLabel.setText(rightTitle);
		rightPanel.add(rightLabel, BorderLayout.NORTH);
	}

	public void removeLeftTitle()
	{
		if (leftLabel != null)
		{
			leftPanel.remove(leftLabel);
			leftLabel = null;
		}
	}

	public void removeRightTitle()
	{
		if (rightLabel != null)
		{
			rightPanel.remove(rightLabel);
			rightLabel = null;
		}
	}

	public int getLeftSize()
	{
		return leftModel.getSize();
	}

	public int getRightSize()
	{
		return rightModel.getSize();
	}

	public Iterator<E> getLeftDataIterator()
	{
		return leftModel.iterator();
	}

	public Iterator<E> getRightDataIterator()
	{
		return rightModel.iterator();
	}

	public void moveAllToLeft()
	{
		leftModel.addAll(rightModel.data);
		rightModel.clear();
	}

	public void moveAllToRight()
	{
		rightModel.addAll(leftModel.data);
		leftModel.clear();
	}

	private static class MyListModel<E> extends AbstractListModel implements Iterable<E>
	{
		private List<E> data;

		private MyListModel(List<E> data)
		{
			this.data = data;
		}

		public int getSize()
		{
			return data.size();
		}

		public Object getElementAt(int index)
		{
			return data.get(index);
		}

		public Iterator<E> iterator()
		{
			return data.iterator();
		}

		public void clear()
		{
			if (data.isEmpty())
				return;
			int i = data.size();
			data.clear();
			fireIntervalRemoved(this, 0, i - 1);
		}

		public void addAll(Collection<E> newData)
		{
			int i = data.size();
			data.addAll(newData);
			fireIntervalAdded(this, i, i + newData.size() - 1);
		}

		public void remove(int index)
		{
			data.remove(index);

			fireContentsChanged(this, index, index);
		}

		public void add(int pos, E[] addedDatas)
		{
			for (int i = addedDatas.length - 1; i >= 0; i--)
				data.add(pos, addedDatas[i]);

			fireContentsChanged(this, pos, pos + addedDatas.length - 1);
		}
	}

	private class MyTransferHandler extends TransferHandler
	{
		private JList sourceList;
		private int[]indices;

		@Override
		public int getSourceActions(JComponent c)
		{
			return MOVE;
		}

		@Override
		public boolean importData(JComponent comp, Transferable t)
		{
			try
			{
				@SuppressWarnings({"unchecked"})
				E[] transferData = (E[]) t.getTransferData(MyTransferable.javaListFlavor);
				JList targetList = (JList) comp;
				@SuppressWarnings({"unchecked"})
				MyListModel<E> targetModel = (MyListModel<E>) targetList.getModel();
				@SuppressWarnings({"unchecked"})
				MyListModel<E> sourceModel = (MyListModel<E>) sourceList.getModel();
				int dropLocation = targetList.getSelectedIndex();
				if(dropLocation == -1)dropLocation=0;
				targetModel.add(dropLocation, transferData);
				int dropStart = dropLocation;
				if (targetList == sourceList)
				{
					
					for (int i = indices.length - 1; i >= 0; i--)
					{
						int index = indices[i];
						if (indices[i] >= dropLocation)
						{
							index += transferData.length;
						}
						else
						{
							dropStart--;
						}
						sourceModel.remove(index);
					}
					for (int i = indices.length - 1; i >= 0; i--)
					{
						indices[i] = dropStart + i;
					}
				}
				else
				{
					
					sourceList.clearSelection();
					for (int i = indices.length - 1; i >= 0; i--)
					{
						sourceModel.remove(indices[i]);
						indices[i] = dropLocation + i;
					}
				}
				targetList.setSelectedIndices(indices);
				return true;
			}
			catch (UnsupportedFlavorException e)
			{
				Log.log(Log.ERROR, this, e);
			}
			catch (IOException e)
			{
				Log.log(Log.ERROR, this, e);
			}
			return false;
		}

		@Override
		protected Transferable createTransferable(JComponent c)
		{
			sourceList = (JList) c;
			indices = sourceList.getSelectedIndices();

			@SuppressWarnings({"unchecked"})
			E[] objects = (E[]) sourceList.getSelectedValues();
			return new MyTransferable<E>(objects);
		}

		@Override
		public boolean canImport(JComponent comp, DataFlavor[] transferFlavors)
		{
			return comp == left || comp == right;
		}
	}

	private static class MyTransferable<E> implements Transferable
	{
		public static final DataFlavor javaListFlavor = new DataFlavor(Collection.class, "java.util.Collection");

		private final E[] data;

		private MyTransferable(E[] data)
		{
			this.data = data;
		}

		public DataFlavor[] getTransferDataFlavors()
		{
			return new DataFlavor[]{javaListFlavor};
		}

		public boolean isDataFlavorSupported(DataFlavor flavor)
		{
			return flavor.equals(javaListFlavor);
		}

		public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException
		{
			return data;
		}
	}
}
