

package org.gjt.sp.jedit.textarea;


import java.awt.AWTEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.awt.Point;
import java.awt.event.MouseEvent;
import javax.swing.JMenuItem;

import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.options.GlobalOptions;

import org.gjt.sp.jedit.msg.PositionChanging;



public class JEditTextArea extends TextArea
{

	
	
	public JEditTextArea(View view)
	{
		super(jEdit.getPropertyManager(), view);
		enableEvents(AWTEvent.FOCUS_EVENT_MASK | AWTEvent.KEY_EVENT_MASK);
		this.view = view;
	} 

	
	@Override
	public FoldPainter getFoldPainter()
	{
		FoldPainter foldPainter = (FoldPainter) ServiceManager.getService(
				FOLD_PAINTER_SERVICE, getFoldPainterName());
		if (foldPainter == null)
			foldPainter = (FoldPainter) ServiceManager.getService(
				FOLD_PAINTER_SERVICE,
				DEFAULT_FOLD_PAINTER_SERVICE);
		return foldPainter;
	} 

	
	
	@Override
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

	
	
	@Override
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

	
	public void goToBufferEnd(boolean select)
	{
		EditBus.send(new PositionChanging(this));
		super.goToBufferEnd(select);
	}

	
	
	public void goToMatchingBracket()
	{
		if(getLineLength(caretLine) != 0)
		{
			int dot = caret - getLineStartOffset(caretLine);

			int bracket = TextUtilities.findMatchingBracket(
				buffer,caretLine,Math.max(0,dot - 1));
			if(bracket != -1)
			{
				EditBus.send(new PositionChanging(this));
				selectNone();
				moveCaretPosition(bracket + 1,false);
				return;
			}
		}
		getToolkit().beep();
	} 


	public void goToBufferStart(boolean select)
	{
		EditBus.send(new PositionChanging(this));
		super.goToBufferStart(select);
	} 

	
	@Override
	public int replaceSelection(String selectedText)
	{
		EditBus.send(new PositionChanging(this));
		return super.replaceSelection(selectedText);
	}

	
	
	public void showGoToLineDialog()
	{
		String line = GUIUtilities.input(view,"goto-line",null);
		if(line == null)
			return;

		try
		{
			int lineNumber = Integer.parseInt(line) - 1;
			EditBus.send(new PositionChanging(this));
			setCaretPosition(getLineStartOffset(lineNumber));
		}
		catch(Exception e)
		{
			getToolkit().beep();
		}
	} 

	
	
	@Override
	public void userInput(char ch)
	{
		if(ch == ' ' && Abbrevs.getExpandOnInput()
			&& Abbrevs.expandAbbrev(view,false))
			return;

		super.userInput(ch);
	} 

	
	
	@Override
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

	
	
	@Override
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

	
	protected static void doWordCount(View view, String text)
	{
		char[] chars = text.toCharArray();
		int characters = chars.length;
		int words = 0;
		int lines = 1;

		boolean word = true;
		for(int i = 0; i < chars.length; i++)
		{
			switch(chars[i])
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

		Object[] args = { characters, words, lines };
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

		doWordCount(view,buffer.getText(0,buffer.getLength()));
	} 

	

	
	
	public View getView()
	{
		return view;
	} 

	

	

	
	
	@Deprecated
	public final int getSelectionStart()
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getStart();
	} 

	
	
	@Deprecated
	public int getSelectionStart(int line)
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getStart(buffer,line);
	} 

	
	
	@Deprecated
	public final int getSelectionStartLine()
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getStartLine();
	} 

	
	
	@Deprecated
	public final void setSelectionStart(int selectionStart)
	{
		int selectionEnd = getSelectionCount() == 1 ? getSelection(0).getEnd() : caret;
		setSelection(new Selection.Range(selectionStart, selectionEnd));
		moveCaretPosition(selectionEnd,true);
	} 

	
	
	@Deprecated
	public final int getSelectionEnd()
	{
		return getSelectionCount() == 1 ? getSelection(0).getEnd() : caret;

	} 

	
	
	@Deprecated
	public int getSelectionEnd(int line)
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getEnd(buffer,line);
	} 

	
	
	@Deprecated
	public final int getSelectionEndLine()
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getEndLine();
	} 

	
	
	@Deprecated
	public final void setSelectionEnd(int selectionEnd)
	{
		int selectionStart = getSelectionCount() == 1 ?	getSelection(0).getStart() : caret;
		setSelection(new Selection.Range(selectionStart, selectionEnd));
		moveCaretPosition(selectionEnd,true);
	} 

	
	
	@Deprecated
	public void select(int start, int end)
	{
		setSelection(new Selection.Range(start, end));
		moveCaretPosition(end,true);
	} 

	
	
	@Deprecated
	public void select(int start, int end, boolean doElectricScroll)
	{
		selectNone();

		int newStart, newEnd;
		if(start < end)
		{
			newStart = start;
			newEnd = end;
		}
		else
		{
			newStart = end;
			newEnd = start;
		}

		setSelection(new Selection.Range(newStart,newEnd));
		moveCaretPosition(end,doElectricScroll);
	} 

	
	
	@Deprecated
	public boolean isSelectionRectangular()
	{
		Selection s = getSelectionAtOffset(caret);
		return s != null && s instanceof Selection.Rect;
	} 

	

	

	
	private View view;
	
	

	
	
	public static final String FOLD_PAINTER_PROPERTY = "foldPainter";
	public static final String FOLD_PAINTER_SERVICE = "org.gjt.sp.jedit.textarea.FoldPainter";
	public static final String DEFAULT_FOLD_PAINTER_SERVICE = "Triangle";

	
	public static String getFoldPainterName()
	{
		return jEdit.getProperty(FOLD_PAINTER_PROPERTY, DEFAULT_FOLD_PAINTER_SERVICE);
	} 

	

	
	
	@Override
	public void handlePopupTrigger(MouseEvent evt)
	{

		if(popup.isVisible())
			popup.setVisible(false);
		else
		{
			
			createPopupMenu(evt);

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

	
	
	@Override
	public void createPopupMenu(MouseEvent evt)
	{
		popup = GUIUtilities.loadPopupMenu("view.context", this, evt);
		JMenuItem customize = new JMenuItem(jEdit.getProperty(
			"view.context.customize"));
		customize.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				new GlobalOptions(view,"context");
			}
		});
		popup.addSeparator();
		popup.add(customize);
	} 

	
	
	@Override
	public void showPopupMenu()
	{
		if (!popup.isVisible() && hasFocus())
		{
			Point caretPos = offsetToXY(getCaretPosition());
			if (caretPos != null)
			{
				
				int charHeight = getPainter().getFontMetrics().getHeight();
				GUIUtilities.showPopupMenu(popup,
					painter,caretPos.x,caretPos.y + charHeight,true);
			}
		}
	} 


}
