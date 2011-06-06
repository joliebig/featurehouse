

package org.gjt.sp.jedit;

import java.util.*;


public class OptionGroup
{
	
	
	protected final String name;
	protected final String label;
	protected final Vector members;
	private boolean sort;
	
	
	
	
	public OptionGroup(String name)
	{
		this.name = name;
		label = jEdit.getProperty("options." + name + ".label");
		members = new Vector();
	} 

	
	
	public OptionGroup(String name, String label, String options)
	{
		this.name = name;
		this.label = label;
		members = new Vector();

		StringTokenizer st = new StringTokenizer(options);
		while(st.hasMoreTokens())
		{
			String pane = st.nextToken();
			addOptionPane(pane);
		}
	} 

	
	public String getName()
	{
		return name;
	} 

	
	
	public String getLabel()
	{
		return label;
	} 

	
	public void addOptionGroup(OptionGroup group)
	{
		insertionSort(group.getLabel(),group);
	} 

	
	public void addOptionPane(OptionPane pane)
	{
		String label = jEdit.getProperty("options."
			+ pane.getName() + ".label","NO LABEL PROPERTY: "
			+ pane.getName());

		insertionSort(label,pane);
	} 

	
	public void addOptionPane(String pane)
	{
		String label = jEdit.getProperty("options."
			+ pane + ".label","NO LABEL PROPERTY: "
			+ pane);

		insertionSort(label,pane);
	} 

	
	public Enumeration getMembers()
	{
		return members.elements();
	} 

	
	public Object getMember(int index)
	{
		return (index >= 0 && index < members.size())
			? members.elementAt(index) : null;
	} 

	
	public int getMemberIndex(Object member)
	{
		return members.indexOf(member);
	} 

	
	public int getMemberCount()
	{
		return members.size();
	} 

	
	
	public void setSort(boolean sort)
	{
		this.sort = sort;
	} 

	


	
	private void insertionSort(String newLabel, Object newObj)
	{
		if(sort)
		{
			for(int i = 0; i < members.size(); i++)
			{
				Object obj = members.elementAt(i);
				String label;
				if(obj instanceof OptionPane)
				{
					String name = ((OptionPane)obj).getName();
					label = jEdit.getProperty("options."
						+ name + ".label","NO LABEL PROPERTY: "
						+ name);
				}
				else if(obj instanceof String)
				{
					label = jEdit.getProperty("options."
						+ obj + ".label","NO LABEL PROPERTY: "
						+ obj);
				}
				else if(obj instanceof OptionGroup)
					label = ((OptionGroup)obj).getLabel();
				else
					throw new InternalError();

				if(newLabel.compareToIgnoreCase(label) < 0)
				{
					members.insertElementAt(newObj,i);
					return;
				}
			}
		}

		members.addElement(newObj);
	} 

	
}
