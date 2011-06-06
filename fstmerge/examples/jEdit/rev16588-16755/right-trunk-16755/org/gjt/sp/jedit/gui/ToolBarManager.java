

package org.gjt.sp.jedit.gui;


import java.awt.*;
import java.util.*;
import java.util.List;

import org.gjt.sp.jedit.*;


public class ToolBarManager
{
	
	public ToolBarManager(Container top, Container bottom)
	{
		this.top = top;
		this.bottom = bottom;
	} 

	
	public void addToolBar(int group, int layer, Component toolbar)
	{
		Entry entry = new Entry(layer, toolbar);

		if (group == View.TOP_GROUP)
			addToolBar(top, topToolBars, entry);
		else if (group == View.BOTTOM_GROUP)
			addToolBar(bottom, bottomToolBars, entry);
		else
			throw new InternalError("Invalid tool bar group");
	} 

	
	public void removeToolBar(Component toolbar)
	{
		removeToolBar(top, topToolBars, toolbar);
		removeToolBar(bottom, bottomToolBars, toolbar);
	} 

	

	
	private Container top;
	private Container bottom;

	private List<Entry> topToolBars = new ArrayList<Entry>();
	private List<Entry> bottomToolBars = new ArrayList<Entry>();
	

	
	private static void addToolBar(Container group, List<Entry> toolbars,
		Entry entry)
	{
		
		for(int i = 0; i < toolbars.size(); i++)
		{
			if(entry.layer > toolbars.get(i).layer)
			{
				toolbars.add(i,entry);
				group.add(entry.toolbar,i);
				return;
			}
		}

		
		toolbars.add(entry);
		group.add(entry.toolbar);
	} 

	
	private static void removeToolBar(Container group, List<Entry> toolbars,
		Component toolbar)
	{
		for(int i = 0; i < toolbars.size(); i++)
		{
			if(toolbar == toolbars.get(i).toolbar)
			{
				group.remove(toolbar);
				toolbars.remove(i);

				return;
			}
		}
	} 

	

	
	private static class Entry
	{
		int layer;
		Component toolbar;

		Entry(int layer, Component toolbar)
		{
			this.layer = layer;
			this.toolbar = toolbar;
		}
	} 
}
