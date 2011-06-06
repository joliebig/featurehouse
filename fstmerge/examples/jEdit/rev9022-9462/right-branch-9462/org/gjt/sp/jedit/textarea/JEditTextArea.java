

package org.gjt.sp.jedit.textarea;


import java.awt.*;
import java.awt.event.MouseEvent;

import org.gjt.sp.jedit.*;

import javax.swing.*;



public class JEditTextArea extends TextArea
{
	
	
	public JEditTextArea(View view)
	{
		super(view);
		enableEvents(AWTEvent.FOCUS_EVENT_MASK | AWTEvent.KEY_EVENT_MASK);
		popupEnabled = true;
		this.view = view;
	} 

	
	
	public void smartHome(boolean select)
	{
		Macros.Recorder recorder = view.getMacroRecorder();

		switch(view.getInputHandler().getLastActionCount())
		{
		case 1:
			if(recorder != null)
				recorder.record("textArea.goToStartOfWhiteSpace(" + select + ");");

			goToStartOfWhiteSpace(select);
			break;
		case 2:
			if(recorder != null)
				recorder.record("textArea.goToStartOfLine(" + select + ");");

			goToStartOfLine(select);
			break;
		default: 
			if(recorder != null)
				recorder.record("textArea.goToFirstVisibleLine(" + select + ");");

			goToFirstVisibleLine(select);
			break;
		}
	} 

	
	
	public void smartEnd(boolean select)
	{
		Macros.Recorder recorder = view.getMacroRecorder();

		switch(view.getInputHandler().getLastActionCount())
		{
		case 1:
			if(recorder != null)
				recorder.record("textArea.goToEndOfWhiteSpace(" + select + ");");

			goToEndOfWhiteSpace(select);
			break;
		case 2:
			if(recorder != null)
				recorder.record("textArea.goToEndOfLine(" + select + ");");

			goToEndOfLine(select);
			break;
		default: 
			if(recorder != null)
				recorder.record("textArea.goToLastVisibleLine(" + select + ");");
			goToLastVisibleLine(select);
			break;
		}
	} 

	
	
	public void showGoToLineDialog()
	{
		String line = GUIUtilities.input(view,"goto-line",null);
		if(line == null)
			return;

		try
		{
			int lineNumber = Integer.parseInt(line) - 1;
			setCaretPosition(getLineStartOffset(lineNumber));
		}
		catch(Exception e)
		{
			getToolkit().beep();
		}
	} 

	
	
	public void userInput(char ch)
	{
		if(!isEditable())
		{
			getToolkit().beep();
			return;
		}

		
		if(hiddenCursor != null)
			getPainter().setCursor(hiddenCursor);

		if(ch == ' ' && Abbrevs.getExpandOnInput()
			&& Abbrevs.expandAbbrev(view,false))
			return;

		if(ch == '\t')
			userInputTab();
		else
		{
			boolean indent = buffer.isElectricKey(ch, caretLine);
			String str = String.valueOf(ch);
			if(getSelectionCount() == 0)
			{
				if(!doWordWrap(ch == ' '))
					insert(str,indent);
			}
			else
				replaceSelection(str);
		}
	} 

	
	
	public void addExplicitFold()
	{
		try
		{
			super.addExplicitFold();
		}
		catch (TextAreaException e)
		{
			GUIUtilities.error(view,"folding-not-explicit",null);
		}
	} 

	
	
	public void formatParagraph()
	{
		try
		{
			super.formatParagraph();
		}
		catch (TextAreaException e)
		{
			GUIUtilities.error(view,"format-maxlinelen",null);
		}
	} 

	
	protected static void doWordCount(View view, CharSequence text)
	{
		int words = 0;
		int lines = 1;

		boolean word = true;
		for(int i = 0; i < text.length(); i++)
		{
			switch(text.charAt(i))
			{
			case '\r': case '\n':
				lines++;
			case ' ': case '\t':
				word = true;
				break;
			default:
				if(word)
				{
					words++;
					word = false;
				}
				break;
			}
		}

		Object[] args = { text.length(), words, lines };
		GUIUtilities.message(view,"wordcount",args);
	} 

	
	
	public void showWordCountDialog()
	{
		String selection = getSelectedText();
		if(selection != null)
		{
			doWordCount(view,selection);
			return;
		}

		doWordCount(view,getBufferText());
	} 

	

	
	
	public View getView()
	{
		return view;
	} 

	

	

	
	private View view;
	private JPopupMenu popup;
	private boolean popupEnabled;
	
	

	
	
	public boolean isRightClickPopupEnabled()
	{
		return popupEnabled;
	} 

	
	
	public void setRightClickPopupEnabled(boolean popupEnabled)
	{
		this.popupEnabled = popupEnabled;
	} 

	
	
	public final JPopupMenu getRightClickPopup()
	{
		return popup;
	} 

	
	
	public final void setRightClickPopup(JPopupMenu popup)
	{
		this.popup = popup;
	} 

	
	
	public void handlePopupTrigger(MouseEvent evt)
	{
		if(popup.isVisible())
			popup.setVisible(false);
		else
		{
			int x = evt.getX();
			int y = evt.getY();

			int dragStart = xyToOffset(x,y,
				!(painter.isBlockCaretEnabled()
				|| isOverwriteEnabled()));

			if(getSelectionCount() == 0 || multi)
				moveCaretPosition(dragStart,false);
			GUIUtilities.showPopupMenu(popup,painter,x,y);
		}
	} 
}
