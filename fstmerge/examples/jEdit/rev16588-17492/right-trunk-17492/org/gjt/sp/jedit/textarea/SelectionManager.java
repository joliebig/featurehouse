

package org.gjt.sp.jedit.textarea;


import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import org.gjt.sp.jedit.buffer.*;


class SelectionManager
{
	
	
	List<Selection> selection;

	
	SelectionManager(TextArea textArea)
	{
		this.textArea = textArea;
		selection = new ArrayList<Selection>();
	} 

	
	
	int getSelectionCount()
	{
		return selection.size();
	} 

	
	
	public Selection[] getSelection()
	{
		return selection.toArray(
			new Selection[selection.size()]);
	} 

	
	
	void setSelection(Selection[] selection)
	{
		this.selection.clear();
		addToSelection(selection);
	} 

	
	
	void addToSelection(Selection[] selection)
	{
		if(selection != null)
		{
			for(int i = 0; i < selection.length; i++)
			{
				Selection s = selection[i];
				if(s != null)
					addToSelection(s);
			}
		}
	} 

	
	void addToSelection(Selection addMe)
	{
		if(addMe.start > addMe.end)
		{
			throw new IllegalArgumentException(addMe.start
				+ " > " + addMe.end);
		}
		else if(addMe.start == addMe.end)
		{
			if(addMe instanceof Selection.Range)
				return;
			else if(addMe instanceof Selection.Rect)
			{
				if(((Selection.Rect)addMe).extraEndVirt == 0)
					return;
			}
		}

		Iterator<Selection> iter = selection.iterator();
		while(iter.hasNext())
		{
			
			
			Selection s = iter.next();
			if(s.overlaps(addMe))
			{
				addMe.start = Math.min(s.start,addMe.start);
				addMe.end = Math.max(s.end,addMe.end);
				iter.remove();
			}
		}

		addMe.startLine = textArea.getLineOfOffset(addMe.start);
		addMe.endLine = textArea.getLineOfOffset(addMe.end);

		boolean added = false;

		for(int i = 0; i < selection.size(); i++)
		{
			Selection s = selection.get(i);
			if(addMe.start < s.start)
			{
				selection.add(i,addMe);
				added = true;
				break;
			}
		}

		if(!added)
			selection.add(addMe);

		textArea.invalidateLineRange(addMe.startLine,addMe.endLine);
	} 

	
	
	void setSelection(Selection selection)
	{
		this.selection.clear();

		if(selection != null)
			addToSelection(selection);
	} 

	
	
	Selection getSelectionAtOffset(int offset)
	{
		if(selection != null)
		{
			for (Selection s : selection)
			{
				if(offset >= s.start && offset <= s.end)
					return s;
			}
		}

		return null;
	} 

	
	
	void removeFromSelection(Selection sel)
	{
		selection.remove(sel);
	} 

	
	
	void resizeSelection(int offset, int end, int extraEndVirt,
		boolean rect)
	{
		boolean reversed = false;
		if(end < offset)
		{
			int tmp = offset;
			offset = end;
			end = tmp;
			reversed = true;
		}

		Selection newSel;
		if(rect)
		{
			Selection.Rect rectSel = new Selection.Rect(offset,end);
			if(reversed)
				rectSel.extraStartVirt = extraEndVirt;
			else
				rectSel.extraEndVirt = extraEndVirt;
			newSel = rectSel;
		}
		else
			newSel = new Selection.Range(offset,end);

		addToSelection(newSel);
	} 

	
	
	int[] getSelectedLines()
	{

		Set<Integer> set = new TreeSet<Integer>();
		for (Selection s : selection)
		{
			int endLine =
				s.end == textArea.getLineStartOffset(s.endLine)
				? s.endLine - 1
				: s.endLine;

			for(int j = s.startLine; j <= endLine; j++)
			{
				set.add(j);
			}
		}


		int[] returnValue = new int[set.size()];
		int i = 0;
		for (Integer line : set)
			returnValue[i++] = line;

		return returnValue;
	} 
	
	
	void invertSelection()
	{
		Selection[] newSelection = new Selection[selection.size() + 1];
		int lastOffset = 0;
		for(int i = 0; i < selection.size(); i++)
		{
			Selection s = selection.get(i);
			newSelection[i] = new Selection.Range(lastOffset,
				s.getStart());
			lastOffset = s.getEnd();
		}
		newSelection[selection.size()] = new Selection.Range(
			lastOffset,textArea.getBufferLength());
		setSelection(newSelection);
	} 

	
	
	int[] getSelectionStartAndEnd(int screenLine, int physicalLine,
		Selection s)
	{
		int start = textArea.getScreenLineStartOffset(screenLine);
		int end = textArea.getScreenLineEndOffset(screenLine);

		if(end <= s.start || start > s.end)
			return null;

		int selStartScreenLine;
		if(textArea.displayManager.isLineVisible(s.startLine))
			selStartScreenLine = textArea.getScreenLineOfOffset(s.start);
		else
			selStartScreenLine = -1;

		int selEndScreenLine;
		if(textArea.displayManager.isLineVisible(s.endLine))
			selEndScreenLine = textArea.getScreenLineOfOffset(s.end);
		else
			selEndScreenLine = -1;

		JEditBuffer buffer = textArea.getBuffer();

		int lineStart = buffer.getLineStartOffset(physicalLine);
		int x1, x2;

		if(s instanceof Selection.Rect)
		{
			start -= lineStart;
			end -= lineStart;

			Selection.Rect rect = (Selection.Rect)s;
			int _start = rect.getStartColumn(buffer);
			int _end = rect.getEndColumn(buffer);

			int lineLen = buffer.getLineLength(physicalLine);

			int[] total = new int[1];

			int rectStart = buffer.getOffsetOfVirtualColumn(
				physicalLine,_start,total);
			if(rectStart == -1)
			{
				x1 = (_start - total[0]) * textArea.charWidth;
				rectStart = lineLen;
			}
			else
				x1 = 0;

			int rectEnd = buffer.getOffsetOfVirtualColumn(
				physicalLine,_end,total);
			if(rectEnd == -1)
			{
				x2 = (_end - total[0]) * textArea.charWidth;
				rectEnd = lineLen;
			}
			else
				x2 = 0;

			if(end <= rectStart || start > rectEnd)
				return null;

			x1 = rectStart < start ? 0
				: x1 + textArea.offsetToXY(physicalLine,
				rectStart).x;
			x2 = rectEnd > end ? textArea.getWidth()
				: x2 + textArea.offsetToXY(physicalLine,
				rectEnd).x;
		}
		else if(selStartScreenLine == selEndScreenLine
			&& selStartScreenLine != -1)
		{
			x1 = textArea.offsetToXY(physicalLine,
				s.start - lineStart).x;
			x2 = textArea.offsetToXY(physicalLine,
				s.end - lineStart).x;
		}
		else if(screenLine == selStartScreenLine)
		{
			x1 = textArea.offsetToXY(physicalLine,
				s.start - lineStart).x;
			x2 = textArea.getWidth();
		}
		else if(screenLine == selEndScreenLine)
		{
			x1 = 0;
			x2 = textArea.offsetToXY(physicalLine,
				s.end - lineStart).x;
		}
		else
		{
			x1 = 0;
			x2 = textArea.getWidth();
		}

		if(x1 < 0)
			x1 = 0;
		if(x2 < 0)
			x2 = 0;

		if(x1 == x2)
			x2++;

		return new int[] { x1, x2 };
	} 

	
	
	boolean insideSelection(int x, int y)
	{
		int offset = textArea.xyToOffset(x,y);

		Selection s = textArea.getSelectionAtOffset(offset);
		if(s == null)
			return false;

		int screenLine = textArea.getScreenLineOfOffset(offset);
		if(screenLine == -1)
			return false;

		int[] selectionStartAndEnd = getSelectionStartAndEnd(
			screenLine,textArea.getLineOfOffset(offset),s);
		if(selectionStartAndEnd == null)
			return false;

		return x >= selectionStartAndEnd[0]
			&& x <= selectionStartAndEnd[1];
	} 

	private TextArea textArea;
}
