

package org.gjt.sp.jedit.textarea;


import java.awt.event.*;

import org.gjt.sp.jedit.Registers;
import org.gjt.sp.jedit.OperatingSystem;



public class MouseHandler extends TextAreaMouseHandler
{
	
	public MouseHandler(JEditTextArea textArea)
	{
		super(textArea);
		this.textArea = textArea;
	} 

	
	public void mousePressed(MouseEvent evt)
	{
		showCursor();

		control = (OperatingSystem.isMacOS() && evt.isMetaDown())
			|| (!OperatingSystem.isMacOS() && evt.isControlDown());

		
		
		textArea.getInputHandler().resetLastActionCount();

		quickCopyDrag = (textArea.isQuickCopyEnabled() &&
			isMiddleButton(evt.getModifiers()));

		if(!quickCopyDrag)
		{
			textArea.requestFocus();
			TextArea.focusedComponent = textArea;
		}

		if(textArea.getBuffer().isLoading())
			return;

		int x = evt.getX();
		int y = evt.getY();

		dragStart = textArea.xyToOffset(x,y,
			!(textArea.getPainter().isBlockCaretEnabled()
			|| textArea.isOverwriteEnabled()));
		dragStartLine = textArea.getLineOfOffset(dragStart);
		dragStartOffset = dragStart - textArea.getLineStartOffset(
			dragStartLine);

		if(isPopupTrigger(evt)
			&& textArea.getRightClickPopup() != null)
		{
			if(textArea.isRightClickPopupEnabled())
				textArea.handlePopupTrigger(evt);
			return;
		}

		dragged = false;

		textArea.blink = true;
		textArea.invalidateLine(textArea.getCaretLine());

		clickCount = evt.getClickCount();

		if(textArea.isDragEnabled()
			&& textArea.selectionManager.insideSelection(x,y)
			&& clickCount == 1 && !evt.isShiftDown())
		{
			maybeDragAndDrop = true;
			textArea.moveCaretPosition(dragStart,false);
			return;
		}

		maybeDragAndDrop = false;

		if(quickCopyDrag)
		{
			
			doSingleClick(evt);
		}
		else
		{
			switch(clickCount)
			{
			case 1:
				doSingleClick(evt);
				break;
			case 2:
				doDoubleClick();
				break;
			default: 
				doTripleClick();
				break;
			}
		}
	} 

	
	public void mouseReleased(MouseEvent evt)
	{
		
		
		Selection sel = textArea.getSelectionAtOffset(dragStart);
		if(dragged && sel != null)
		{
			Registers.setRegister('%',textArea.getSelectedText(sel));
			if(quickCopyDrag)
			{
				textArea.removeFromSelection(sel);
				Registers.paste((JEditTextArea) TextArea.focusedComponent,
					'%',sel instanceof Selection.Rect);

				TextArea.focusedComponent.requestFocus();
			}
		}
		else if(!dragged && textArea.isQuickCopyEnabled() &&
			isMiddleButton(evt.getModifiers()))
		{
			textArea.requestFocus();
			TextArea.focusedComponent = textArea;

			textArea.setCaretPosition(dragStart,false);
			if(!textArea.isEditable())
				textArea.getToolkit().beep();
			else
				Registers.paste((JEditTextArea) textArea,'%',control);
		}
		else if(maybeDragAndDrop
			&& !textArea.isMultipleSelectionEnabled())
		{
			textArea.selectNone();
		}

		dragged = false;
	} 

	
	private JEditTextArea textArea;
	
}
