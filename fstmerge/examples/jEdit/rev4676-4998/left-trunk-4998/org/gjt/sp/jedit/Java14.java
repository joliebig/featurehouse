

package org.gjt.sp.jedit;


import javax.swing.event.*;
import javax.swing.*;
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.awt.event.*;
import java.awt.*;
import java.io.File;
import java.util.Iterator;
import java.util.List;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class Java14
{
	
	public static void init()
	{
		JFrame.setDefaultLookAndFeelDecorated(
			jEdit.getBooleanProperty("decorate.frames"));
		JDialog.setDefaultLookAndFeelDecorated(
			jEdit.getBooleanProperty("decorate.dialogs"));

		KeyboardFocusManager.setCurrentKeyboardFocusManager(
			new MyFocusManager());

		EditBus.addToBus(new EBComponent()
		{
			public void handleMessage(EBMessage msg)
			{
				if(msg instanceof ViewUpdate)
				{
					ViewUpdate vu = (ViewUpdate)msg;
					if(vu.getWhat() == ViewUpdate.CREATED)
					{
						vu.getView().setFocusTraversalPolicy(
							new MyFocusTraversalPolicy());
					}
				}
				else if(msg instanceof EditPaneUpdate)
				{
					EditPaneUpdate eu = (EditPaneUpdate)msg;
					if(eu.getWhat() == EditPaneUpdate.CREATED)
					{
						initTextArea(eu.getEditPane()
							.getTextArea());
					}
				}
			}
		});

		Clipboard selection = Toolkit.getDefaultToolkit().getSystemSelection();
		if(selection != null)
		{
			Log.log(Log.DEBUG,Java14.class,"Setting % register"
				+ " to system selection");
			Registers.setRegister('%',new Registers.ClipboardRegister(selection));
		}
	} 

	
	
	public static void dragAndDropCallback(JEditTextArea textArea,
		InputEvent evt, boolean copy)
	{
		Log.log(Log.DEBUG,Java14.class,"Drag and drop callback");
		TransferHandler handler = textArea.getTransferHandler();
		handler.exportAsDrag(textArea,evt,
			copy ? TransferHandler.COPY
			: TransferHandler.MOVE);
	} 

	
	static void initTextArea(JEditTextArea textArea)
	{
		textArea.addMouseWheelListener(new MouseWheelHandler());

		
		
		
		
		textArea.setTransferHandler(new TextAreaTransferHandler());

		try
		{
			textArea.getDropTarget().addDropTargetListener(
				new DropHandler(textArea));
			textArea.setDragAndDropCallback(
				Java14.class.getMethod("dragAndDropCallback",
				new Class[] { JEditTextArea.class,
				InputEvent.class, boolean.class }));
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,Java14.class,e);
		}
	} 

	
	public static void initBufferSwitcher(final EditPane pane,
		BufferSwitcher switcher)
	{
		switcher.addPopupMenuListener(new PopupMenuListener()
		{
			public void popupMenuWillBecomeVisible(
				PopupMenuEvent e) {}

			public void popupMenuWillBecomeInvisible(
				PopupMenuEvent e)
			{
				pane.getTextArea().requestFocus();
			}

			public void popupMenuCanceled(PopupMenuEvent e)
			{
				pane.getTextArea().requestFocus();
			}
		});
	} 

	
	static class MyFocusManager extends DefaultKeyboardFocusManager
	{
		MyFocusManager()
		{
			setDefaultFocusTraversalPolicy(new LayoutFocusTraversalPolicy());
		}

		public boolean postProcessKeyEvent(KeyEvent evt)
		{
			if(!evt.isConsumed())
			{
				Component comp = (Component)evt.getSource();
				if(!comp.isShowing())
					return true;

				for(;;)
				{
					if(comp instanceof View)
					{
						((View)comp).processKeyEvent(evt,
							View.VIEW);
						return true;
					}
					else if(comp == null || comp instanceof Window
						|| comp instanceof JEditTextArea)
					{
						break;
					}
					else
						comp = comp.getParent();
				}
			}

			return super.postProcessKeyEvent(evt);
		}
	} 

	
	static class MyFocusTraversalPolicy extends LayoutFocusTraversalPolicy
	{
		public Component getDefaultComponent(Container focusCycleRoot)
		{
			return GUIUtilities.getView(focusCycleRoot).getTextArea();
		}
	} 

	
	static class MouseWheelHandler implements MouseWheelListener
	{
		public void mouseWheelMoved(MouseWheelEvent e)
		{
			JEditTextArea textArea = (JEditTextArea)e.getSource();

			
			if(e.isAltDown())
			{
				moveCaret(textArea,e.getWheelRotation(),
					e.isShiftDown() || e.isControlDown());
			}
			else if(e.isShiftDown())
				scrollPage(textArea,e.getWheelRotation());
			else if(e.isControlDown())
				scrollLine(textArea,e.getWheelRotation());
			else if(e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL)
				scrollLine(textArea,e.getUnitsToScroll());
			else
				scrollLine(textArea,3 * e.getWheelRotation());
		}

		private void scrollLine(JEditTextArea textArea, int amt)
		{
			textArea.setFirstLine(textArea.getFirstLine() + amt);
		}

		private void scrollPage(JEditTextArea textArea, int amt)
		{
			if(amt > 0)
				textArea.scrollDownPage();
			else
				textArea.scrollUpPage();
		}

		private void moveCaret(JEditTextArea textArea, int amt, boolean select)
		{
			if (amt < 0)
				textArea.goToPrevLine(select);
			else
				textArea.goToNextLine(select);
		}
	} 

	
	static class TextAreaTransferHandler extends TransferHandler
	{
		
		private static JEditTextArea dragSource;
		private static boolean compoundEdit;

		protected Transferable createTransferable(JComponent c)
		{
			Log.log(Log.DEBUG,this,"createTransferable()");
			JEditTextArea textArea = (JEditTextArea)c;
			if(textArea.getSelectionCount() == 0)
				return null;
			else
			{
				dragSource = textArea;
				return new TextAreaSelection(textArea);
			}
		}

		public int getSourceActions(JComponent c)
		{
			return COPY_OR_MOVE;
		}

		public boolean importData(JComponent c, Transferable t)
		{
			Log.log(Log.DEBUG,this,"Import data");
			if(!canImport(c,t.getTransferDataFlavors()))
				return false;

			boolean returnValue;

			try
			{
				if(t.isDataFlavorSupported(
					DataFlavor.javaFileListFlavor))
				{
					returnValue = importFile(c,t);
				}
				else
				{
					returnValue = importText(c,t);
				}
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,this,e);
				returnValue = false;
			}

			GUIUtilities.getView(c).toFront();
			GUIUtilities.getView(c).requestFocus();
			c.requestFocus();

			return returnValue;
		}

		private boolean importFile(JComponent c, Transferable t)
			throws Exception
		{
			Log.log(Log.DEBUG,this,"=> File list");
			EditPane editPane = (EditPane)
				GUIUtilities.getComponentParent(
				c,EditPane.class);

			Buffer buffer = null;

			Object data = t.getTransferData(
				DataFlavor.javaFileListFlavor);

			Iterator iterator = ((List)data)
				.iterator();

			while(iterator.hasNext())
			{
				File file = (File)
					iterator.next();
				Buffer _buffer = jEdit.openFile(null,
					file.getPath());
				if(_buffer != null)
					buffer = _buffer;
			}

			if(buffer != null)
				editPane.setBuffer(buffer);
			editPane.getView().toFront();
			editPane.getView().requestFocus();
			editPane.requestFocus();

			return true;
		}

		private boolean importText(JComponent c, Transferable t)
			throws Exception
		{
			Log.log(Log.DEBUG,this,"=> String");
			String str = (String)t.getTransferData(
				DataFlavor.stringFlavor);

			JEditTextArea textArea = (JEditTextArea)c;

			if(dragSource != null
				&& textArea.getBuffer()
				== dragSource.getBuffer())
			{
				compoundEdit = true;
				textArea.getBuffer().beginCompoundEdit();
			}

			int caret = textArea.getCaretPosition();
			Selection s = textArea.getSelectionAtOffset(caret);

			
			if(s != null)
			{
				if(textArea == dragSource)
					return false;
				
				int start = s.getStart();
				textArea.setSelectedText(s,str);
				
			}
			
			else
				textArea.getBuffer().insert(caret,str);
			textArea.scrollToCaret(true);

			return true;
		}

		protected void exportDone(JComponent c, Transferable t,
			int action)
		{
			Log.log(Log.DEBUG,this,"Export done");

			JEditTextArea textArea = (JEditTextArea)c;

			try
			{
				if(t == null)
				{
					Log.log(Log.DEBUG,this,"=> Null transferrable");
					textArea.selectNone();
				}
				else if(t.isDataFlavorSupported(
					DataFlavor.stringFlavor))
				{
					Log.log(Log.DEBUG,this,"=> String");
					if(action == MOVE)
						textArea.setSelectedText(null,false);
					else
						textArea.selectNone();
				}
			}
			finally
			{
				if(compoundEdit)
				{
					compoundEdit = false;
					textArea.getBuffer().endCompoundEdit();
				}
			}

			dragSource = null;
		}

		public boolean canImport(JComponent c, DataFlavor[] flavors)
		{
			JEditTextArea textArea = (JEditTextArea)c;

			
			
			boolean returnValue = false;

			for(int i = 0; i < flavors.length; i++)
			{
				if(flavors[i].equals(
					DataFlavor.javaFileListFlavor))
				{
					returnValue = true;
				}
				else if(flavors[i].equals(
					DataFlavor.stringFlavor))
				{
					if(textArea.isEditable())
						returnValue = true;
				}
			}

			Log.log(Log.DEBUG,this,"canImport() returning "
				+ returnValue);
			return returnValue;
		}
	} 

	
	static class DropHandler extends DropTargetAdapter
	{
		JEditTextArea textArea;
		int savedCaret;

		DropHandler(JEditTextArea textArea)
		{
			this.textArea = textArea;
		}

		public void dragEnter(DropTargetDragEvent dtde)
		{
			Log.log(Log.DEBUG,this,"Drag enter");
			textArea.setDragInProgress(true);
			
			savedCaret = textArea.getCaretPosition();
		}

		public void dragOver(DropTargetDragEvent dtde)
		{
			Point p = dtde.getLocation();
			p = SwingUtilities.convertPoint(textArea,p,
				textArea.getPainter());
			int pos = textArea.xyToOffset(p.x,p.y,
				!(textArea.getPainter().isBlockCaretEnabled()
				|| textArea.isOverwriteEnabled()));
			if(pos != -1)
			{
				textArea.moveCaretPosition(pos,
					JEditTextArea.ELECTRIC_SCROLL);
			}
		}

		public void dragExit(DropTargetEvent dtde)
		{
			Log.log(Log.DEBUG,this,"Drag exit");
			textArea.setDragInProgress(false);
			
			textArea.moveCaretPosition(savedCaret,
				JEditTextArea.ELECTRIC_SCROLL);
		}

		public void drop(DropTargetDropEvent dtde)
		{
			Log.log(Log.DEBUG,this,"Drop");
			textArea.setDragInProgress(false);
			
		}
	} 

	
	static class TextAreaSelection extends StringSelection
	{
		JEditTextArea textArea;

		TextAreaSelection(JEditTextArea textArea)
		{
			super(textArea.getSelectedText());
			this.textArea = textArea;
		}
	} 
}
