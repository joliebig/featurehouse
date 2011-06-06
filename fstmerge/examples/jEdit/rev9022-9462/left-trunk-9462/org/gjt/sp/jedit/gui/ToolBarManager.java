

package org.gjt.sp.jedit.gui;


import java.awt.*;
import java.util.*;
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

	private ArrayList topToolBars = new ArrayList();
	private ArrayList bottomToolBars = new ArrayList();
	

	
	private void addToolBar(Container group, ArrayList toolbars,
		Entry entry)
	{
		
		for(int i = 0; i < toolbars.size(); i++)
		{
			if(entry.layer > ((Entry)toolbars.get(i)).layer)
			{
				toolbars.add(i,entry);
				group.add(entry.toolbar,i);
				return;
			}
		}

		
		toolbars.add(entry);
		group.add(entry.toolbar);
	} 

	
	private void removeToolBar(Container group, ArrayList toolbars,
		Component toolbar)
	{
		for(int i = 0; i < toolbars.size(); i++)
		{
			if(toolbar == ((Entry)toolbars.get(i)).toolbar)
			{
				group.remove(toolbar);
				toolbars.remove(i);

				return;
			}
		}
	} 

	

	
	static class Entry
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
