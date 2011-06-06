

package org.gjt.sp.jedit.textarea;


import java.text.AttributedString;
import java.text.AttributedCharacterIterator;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Graphics2D;
import java.awt.FontMetrics;
import java.awt.im.InputMethodRequests;
import java.awt.event.InputMethodListener;
import java.awt.event.InputMethodEvent;
import java.awt.font.TextLayout;
import java.awt.font.TextAttribute;
import java.awt.font.TextHitInfo;




class InputMethodSupport
	extends TextAreaExtension
	implements InputMethodRequests, InputMethodListener
{
	
	private TextArea owner;
	
	private TextLayout composedTextLayout = null;
	
	private int composedCaretX = 0;
	
	private int lastCommittedAt = 0;
	private String lastCommittedText = null;

	public InputMethodSupport(TextArea owner)
	{
		this.owner = owner;
		owner.addInputMethodListener(this);
		owner.getPainter().addExtension(TextAreaPainter.HIGHEST_LAYER, this);
	}


	
	
	private Rectangle getCaretRectangle(int x, int y)
	{
		TextAreaPainter painter = owner.getPainter();
		Point origin = painter.getLocationOnScreen();
		int height = painter.getFontMetrics().getHeight();
		return new Rectangle(origin.x + x, origin.y + y, 0, height);
	}
	


	
	public void paintValidLine(Graphics2D gfx, int screenLine,
				   int physicalLine, int start, int end, int y)
	{
		if(composedTextLayout != null)
		{
			int caret = owner.getCaretPosition();
			if(start <= caret && caret < end)
			{
				TextAreaPainter painter = owner.getPainter();
				
				
				
				
				FontMetrics fm = painter.getFontMetrics();
				int x = owner.offsetToXY(caret).x;
				int width = Math.round(composedTextLayout.getAdvance());
				int height = fm.getHeight();
				int offset_to_baseline = height
					- fm.getLeading() - fm.getDescent();
				int caret_x = x + composedCaretX;

				gfx.setColor(painter.getBackground());
				gfx.fillRect(x, y, width, height);
				gfx.setColor(painter.getForeground());
				composedTextLayout.draw(gfx, x, y + offset_to_baseline);
				gfx.setColor(painter.getCaretColor());
				gfx.drawLine(caret_x, y, caret_x, y + height - 1);
			}
		}
	}
	


	
	public Rectangle getTextLocation(TextHitInfo offset)
	{
		if(composedTextLayout != null)
		{
			
			Point caret = owner.offsetToXY(owner.getCaretPosition());
			return getCaretRectangle(caret.x + composedCaretX, caret.y);
		}
		else
		{
			
			Selection selection_on_caret = owner.getSelectionAtOffset(owner.getCaretPosition());
			if(selection_on_caret != null)
			{
				Point selection_start = owner.offsetToXY(selection_on_caret.getStart());
				return getCaretRectangle(selection_start.x, selection_start.y);
			}
		}
		return null;
	}

	public TextHitInfo getLocationOffset(int x, int y)
	{
		if(composedTextLayout != null)
		{
			Point origin = owner.getPainter().getLocationOnScreen();
			Point caret = owner.offsetToXY(owner.getCaretPosition());
			float local_x = x - origin.x - caret.x;
			float local_y = y - origin.y - caret.y
				- composedTextLayout.getLeading()
				- composedTextLayout.getAscent();
			return composedTextLayout.hitTestChar(local_x, local_y);
		}
		return null;
	}

	public int getInsertPositionOffset()
	{
		return owner.getCaretPosition();
	}

	public AttributedCharacterIterator getCommittedText(int beginIndex , int endIndex
		, AttributedCharacterIterator.Attribute[] attributes)
	{
		return (new AttributedString(owner.getText(beginIndex, endIndex - beginIndex))).getIterator();
	}

	public int getCommittedTextLength()
	{
		return owner.getBufferLength();
	}

	public AttributedCharacterIterator cancelLatestCommittedText(AttributedCharacterIterator.Attribute[] attributes)
	{
		if(lastCommittedText != null)
		{
			int offset = lastCommittedAt;
			int length = lastCommittedText.length();
			String sample = owner.getText(offset, length);
			if(sample != null && sample.equals(lastCommittedText))
			{
				AttributedCharacterIterator canceled = (new AttributedString(sample)).getIterator();
				owner.getBuffer().remove(offset, length);
				owner.setCaretPosition(offset);
				lastCommittedText = null;
				return canceled;
			}
			
			
			lastCommittedText = null;
		}
		return null;
	}

	public AttributedCharacterIterator getSelectedText(AttributedCharacterIterator.Attribute[] attributes)
	{
		Selection selection_on_caret = owner.getSelectionAtOffset(owner.getCaretPosition());
		if(selection_on_caret != null)
		{
			return (new AttributedString(owner.getSelectedText(selection_on_caret))).getIterator();
		}
		return null;
	}
	


	
	public void inputMethodTextChanged(InputMethodEvent event)
	{
		composedTextLayout = null;
		AttributedCharacterIterator text = event.getText();
		if(text != null)
		{
			int committed_count = event.getCommittedCharacterCount();
			if(committed_count > 0)
			{
				lastCommittedText = null;
				lastCommittedAt = owner.getCaretPosition();
				StringBuffer committed = new StringBuffer(committed_count);
				char c;
				int count;
				for(c = text.first(), count = committed_count
					; c != AttributedCharacterIterator.DONE && count > 0
					; c = text.next(), --count)
				{
					owner.userInput(c);
					committed.append(c);
				}
				lastCommittedText = committed.toString();
			}
			int end_index = text.getEndIndex();
			if(committed_count < end_index)
			{
				AttributedString composed = new AttributedString(text, committed_count, end_index);
				TextAreaPainter painter = owner.getPainter();
				composed.addAttribute(TextAttribute.FONT, painter.getFont());
				composedTextLayout = new TextLayout(composed.getIterator()
					, painter.getFontRenderContext());
			}
		}
		
		caretPositionChanged(event);
	}

	public void caretPositionChanged(InputMethodEvent event)
	{
		composedCaretX = 0;
		if(composedTextLayout != null)
		{
			TextHitInfo caret = event.getCaret();
			if(caret != null)
			{
				composedCaretX = Math.round(composedTextLayout.getCaretInfo(caret)[0]);
			}
			
			int insertion_x = owner.offsetToXY(owner.getCaretPosition()).x;
			TextHitInfo visible = event.getVisiblePosition();
			int composed_visible_x = (visible != null)
				? Math.round(composedTextLayout.getCaretInfo(visible)[0])
				: composedCaretX;
			int visible_x = insertion_x + composed_visible_x;
			int painter_width = owner.getPainter().getWidth();
			int adjustment = 0;
			if(visible_x < 0)
			{
				adjustment = visible_x;
			}
			if(visible_x >= painter_width)
			{
				adjustment = visible_x - (painter_width - 1);
			}
			if(adjustment != 0)
			{
				owner.setHorizontalOffset(owner.getHorizontalOffset() - adjustment);
			}
		}
		else
		{
			
			owner.scrollToCaret(false);
		}
		
		int caret_line = owner.getCaretLine();
		owner.invalidateLineRange(caret_line, caret_line + 1);
		event.consume();
	}
	
}
