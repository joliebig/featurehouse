

package org.gjt.sp.jedit.textarea;

import java.awt.Graphics2D;
import java.util.ArrayList;
import org.gjt.sp.util.Log;

class ExtensionManager
{
	
	void addExtension(int layer, TextAreaExtension ext)
	{
		Entry entry = new Entry(layer,ext);

		for(int i = 0; i < extensions.size(); i++)
		{
			int _layer = ((Entry)extensions.get(i)).layer;
			if(layer < _layer)
			{
				extensions.add(i,entry);
				return;
			}
		}

		extensions.add(entry);
	} 

	
	void removeExtension(TextAreaExtension ext)
	{
		for(int i = 0; i < extensions.size(); i++)
		{
			Entry entry = (Entry)extensions.get(i);
			if(entry.ext == ext)
			{
				extensions.remove(i);
				return;
			}
		}
	} 

	
	TextAreaExtension[] getExtensions()
	{
		TextAreaExtension[] retVal = new TextAreaExtension[
			extensions.size()];
		for(int i = 0; i < extensions.size(); i++)
		{
			retVal[i] = ((Entry)extensions.get(i)).ext;
		}
		return retVal;
	} 

	
	void paintValidLine(Graphics2D gfx, int screenLine,
		int physicalLine, int start, int end, int y)
	{
		for(int i = 0; i < extensions.size(); i++)
		{
			TextAreaExtension ext = ((Entry)extensions.get(i)).ext;
			try
			{
				ext.paintValidLine(gfx,screenLine,
					physicalLine,start,end,y);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,t);

				
				
				extensions.remove(i);
				i--;
			}
		}
	} 

	
	void paintInvalidLine(Graphics2D gfx, int screenLine,
		int y)
	{
		for(int i = 0; i < extensions.size(); i++)
		{
			TextAreaExtension ext = ((Entry)extensions.get(i)).ext;
			try
			{
				ext.paintInvalidLine(gfx,screenLine,y);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,t);

				
				
				extensions.remove(i);
				i--;
			}
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

	
	private ArrayList extensions = new ArrayList();
	

	
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
