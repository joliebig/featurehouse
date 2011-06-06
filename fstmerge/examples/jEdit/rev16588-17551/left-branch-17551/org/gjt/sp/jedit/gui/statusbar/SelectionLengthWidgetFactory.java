

package org.gjt.sp.jedit.gui.statusbar;


import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.msg.ViewUpdate;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.jedit.textarea.Selection;

import javax.swing.*;
import javax.swing.event.CaretListener;
import javax.swing.event.CaretEvent;



public class SelectionLengthWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view)
	{
		Widget selectionLengthWidget = new SelectionLengthWidget(view);
		return selectionLengthWidget;
	} 

	
	private static class SelectionLengthWidget implements Widget, EBComponent
	{
		private final SelectionLength selectionLength;
		private final View view;
		private TextArea textArea;

		SelectionLengthWidget(View view)
		{
			this.view = view;
			textArea = view.getTextArea();
			selectionLength = new SelectionLength();
			selectionLength.setForeground(jEdit.getColorProperty("view.status.foreground"));
			selectionLength.setBackground(jEdit.getColorProperty("view.status.background"));
		}

		public JComponent getComponent()
		{
			return selectionLength;
		}

		public void update()
		{
		}

		public void propertiesChanged()
		{
		}

		public void handleMessage(EBMessage message)
		{
			if (message instanceof ViewUpdate)
			{
				ViewUpdate viewUpdate = (ViewUpdate) message;
				if (viewUpdate.getView() == view && viewUpdate.getWhat() == ViewUpdate.EDIT_PANE_CHANGED)
				{
					if (textArea != null)
					{
						textArea.removeCaretListener(selectionLength);
					}
					textArea = view.getTextArea();
					if (selectionLength.visible)
						textArea.addCaretListener(selectionLength);
				}
			}
		}

		private class SelectionLength extends JLabel implements CaretListener
		{
			boolean visible;
			
			@Override
			public void addNotify()
			{
				super.addNotify();
				visible = true;
				textArea.addCaretListener(this);
			} 


			
			@Override
			public void removeNotify()
			{
				visible = false;
				textArea.removeCaretListener(this);
				super.removeNotify();
			} 

			public void caretUpdate(CaretEvent e)
			{
				Selection selection = textArea.getSelectionAtOffset(textArea.getCaretPosition());
				if (selection == null)
				{
					setText("");
				}
				else
				{
					int selectionEnd = selection.getEnd();
					int selectionStart = selection.getStart();
					int len;
					if (selection instanceof Selection.Rect)
					{
						int startLine = selection.getStartLine();
						int endLine = selection.getEndLine();
						JEditTextArea textArea = view.getTextArea();
						int startLineOffset = textArea.getLineStartOffset(startLine);
						int endLineOffset = textArea.getLineStartOffset(endLine);
						int lines = endLine - startLine + 1;
						int columns = (selectionEnd - endLineOffset) -
							(selectionStart - startLineOffset);
						len = lines * columns;
					}
					else
						len = selectionEnd - selectionStart;
					setText(Integer.toString(len));
				}
			}
		}
	} 
}