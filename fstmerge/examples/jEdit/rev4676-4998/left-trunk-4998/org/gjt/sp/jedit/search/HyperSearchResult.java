

package org.gjt.sp.jedit.search;


import javax.swing.text.Position;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.jedit.*;



public class HyperSearchResult
{
	public String path;
	public Buffer buffer;
	public int line;
	public String str; 
	public Occur occur;
	public int occurCount;

	
	public Buffer getBuffer()
	{
		if(buffer == null)
			buffer = jEdit.openFile(null,path);
		return buffer;
	} 

	
	
	public Selection[] getSelection()
	{
		if(buffer == null)
			return null;

		Selection[] returnValue = new Selection[occurCount];
		Occur o = occur;
		int i = 0;
		while(o != null)
		{
			Selection.Range s = new Selection.Range(
				o.startPos.getOffset(),
				o.endPos.getOffset()
			);
			returnValue[i++] = s;
			o = o.next;
		}
		return returnValue;
	} 

	
	public void goTo(final View view)
	{
		if(buffer == null)
			buffer = jEdit.openFile(null,path);

		if(buffer == null)
			return;

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				Selection[] s = getSelection();
				if(s == null)
					return;

				EditPane pane = view.goToBuffer(buffer);
				JEditTextArea textArea = pane.getTextArea();
				if(textArea.isMultipleSelectionEnabled())
					textArea.addToSelection(s);
				else
					textArea.setSelection(s);
                
				textArea.moveCaretPosition(occur.endPos.getOffset());
			}
		});
	} 

	
	public String toString()
	{
		return str;
	} 

	

	
	HyperSearchResult(Buffer buffer, int line)
	{
		path = buffer.getPath();

		if(!buffer.isTemporary())
			bufferOpened(buffer);

		this.line = line;

		str = (line + 1) + ": " + buffer.getLineText(line)
			.replace('\t',' ').trim();
	} 

	
	void bufferOpened(Buffer buffer)
	{
		this.buffer = buffer;
		Occur o = occur;
		while(o != null)
		{
			o.bufferOpened();
			o = o.next;
		}
	} 

	
	void bufferClosed()
	{
		buffer = null;
		Occur o = occur;
		while(o != null)
		{
			o.bufferClosed();
			o = o.next;
		}
	} 

	
	void addOccur(int start, int end)
	{
		Occur o = new Occur(start,end);
		o.next = occur;
		occur = o;
		occurCount++;
	} 

	

	
	public class Occur
	{
		public int start, end;
		public Position startPos, endPos;
		public Occur next;

		
		Occur(int start, int end)
		{
			this.start = start;
			this.end = end;

			if(buffer != null && !buffer.isTemporary())
				bufferOpened();
		} 

		
		void bufferOpened()
		{
			startPos = buffer.createPosition(Math.min(
				buffer.getLength(),start));
			endPos = buffer.createPosition(Math.min(
				buffer.getLength(),end));
		} 

		
		void bufferClosed()
		{
			start = startPos.getOffset();
			end = endPos.getOffset();
			startPos = endPos = null;
		} 
	} 
}
