

package org.gjt.sp.jedit;

import java.util.*;
import org.gjt.sp.util.Log;


public class OptionGroup
{
	
	
	public OptionGroup(String name)
	{
		this.name = name;
		label = jEdit.getProperty("options." + name + ".label");
		members = new Vector();
	} 

	
	
	public OptionGroup(String label, String options)
	{
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
		members.addElement(group);
	} 

	
	public void addOptionPane(OptionPane pane)
	{
		members.addElement(pane);
	} 

	
	public void addOptionPane(String pane)
	{
		members.addElement(pane);
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

	
	private String name;
	private String label;
	private Vector members;
	
}
