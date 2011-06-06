

package org.gjt.sp.jedit.textarea;

import java.awt.Graphics2D;
import java.util.*;
import org.gjt.sp.util.Log;

class ExtensionManager
{
	
	void addExtension(int layer, TextAreaExtension ext)
	{
		Entry entry = new Entry(layer,ext);

		int i = 0;
		Iterator iter = extensions.iterator();
		while(iter.hasNext())
		{
			int _layer = ((Entry)iter.next()).layer;
			if(layer < _layer)
			{
				extensions.add(i,entry);
				return;
			}
			i++;
		}

		extensions.add(entry);
	} 

	
	void removeExtension(TextAreaExtension ext)
	{
		Iterator iter = extensions.iterator();
		while(iter.hasNext())
		{
			if(((Entry)iter.next()).ext == ext)
			{
				iter.remove();
				return;
			}
		}
	} 

	
	TextAreaExtension[] getExtensions()
	{
		TextAreaExtension[] retVal = new TextAreaExtension[
			extensions.size()];
		Iterator iter = extensions.iterator();
		int i = 0;
		while(iter.hasNext())
		{
			retVal[i++] = ((Entry)iter.next()).ext;
		}
		return retVal;
	} 

	
	void paintScreenLineRange(JEditTextArea textArea, Graphics2D gfx,
		int firstLine, int lastLine, int y, int lineHeight)
	{
		try
		{
			int[] physicalLines = new int[lastLine - firstLine + 1];
			int[] start = new int[physicalLines.length];
			int[] end = new int[physicalLines.length];

			for(int i = 0; i < physicalLines.length; i++)
			{
				int screenLine = i + firstLine;
				ChunkCache.LineInfo lineInfo = textArea
					.chunkCache.getLineInfo(screenLine);

				if(lineInfo.physicalLine == -1)
					physicalLines[i] = -1;
				else
				{
					physicalLines[i] = lineInfo.physicalLine;
					start[i] = textArea.getScreenLineStartOffset(screenLine);
					end[i] = textArea.getScreenLineEndOffset(screenLine);
				}
			}

			paintScreenLineRange(gfx,firstLine,lastLine,physicalLines,
				start,end,y,lineHeight);
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,"Error repainting line"
				+ " range {" + firstLine + ","
				+ lastLine + "}:");
			Log.log(Log.ERROR,this,e);
		}
	} 

	
	String getToolTipText(int x, int y)
	{
		for(int i = 0; i < extensions.size(); i++)
		{
			TextAreaExtension ext = ((Entry)extensions.get(i)).ext;
			String toolTip = ext.getToolTipText(x,y);
			if(toolTip != null)
				return toolTip;
		}

		return null;
	} 

	
	private List extensions = new LinkedList();

	
	private void paintScreenLineRange(Graphics2D gfx, int firstLine,
		int lastLine, int[] physicalLines, int[] start, int[] end,
		int y, int lineHeight)
	{
		Iterator iter = extensions.iterator();
		while(iter.hasNext())
		{
			TextAreaExtension ext = ((Entry)iter.next()).ext;
			try
			{
				ext.paintScreenLineRange(gfx,firstLine,lastLine,
					physicalLines,start,end,y,lineHeight);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,t);

				
				
				iter.remove();
			}
		}
	} 

	

	
	static class Entry
	{
		int layer;
		TextAreaExtension ext;

		Entry(int layer, TextAreaExtension ext)
		{
			this.layer = layer;
			this.ext = ext;
		}
	} 
}
