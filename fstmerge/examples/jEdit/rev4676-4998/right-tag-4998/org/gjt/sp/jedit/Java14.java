

package org.gjt.sp.jedit;


import javax.swing.*;
import java.awt.datatransfer.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.EditBus;
import org.gjt.sp.util.Log;



class Java14
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
						eu.getEditPane().getTextArea()
							.addMouseWheelListener(
							new MouseWheelHandler());
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
						((View)comp).processKeyEvent(evt);
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
}
