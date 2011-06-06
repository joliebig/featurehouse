

package org.gjt.sp.jedit;


import java.io.*;
import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;


class ActionListHandler extends DefaultHandler
{
	
	ActionListHandler(String path, ActionSet actionSet)
	{
		this.path = path;
		this.actionSet = actionSet;
		stateStack = new Stack();
		code = new StringBuffer();
		isSelected = new StringBuffer();
	} 

	
	public InputSource resolveEntity(String publicId, String systemId)
	{
		return XMLUtilities.findEntity(systemId, "actions.dtd", getClass());
	} 

	
	public void attribute(String aname, String value, boolean isSpecified)
	{
		aname = (aname == null) ? null : aname.intern();
		value = (value == null) ? null : value.intern();

		if(aname == "NAME")
			actionName = value;
		else if(aname == "NO_REPEAT")
			noRepeat = (value == "TRUE");
		else if(aname == "NO_RECORD")
			noRecord = (value == "TRUE");
		else if(aname == "NO_REMEMBER_LAST")
			noRememberLast = (value == "TRUE");
	} 

	
	public void characters(char[] c, int off, int len)
	{
		String tag = peekElement();
		if (tag.equals("CODE"))
		{
			code.append(c, off, len);
		}
		else if (tag.equals("IS_SELECTED"))
		{
			isSelected.append(c, off, len);
		}
	} 

	
	public void startElement(String uri, String localName,
				 String qName, Attributes attrs)
	{
		String tag = pushElement(qName);

		if (tag.equals("ACTION"))
		{
			actionName = attrs.getValue("NAME");
			noRepeat = "TRUE".equals(attrs.getValue("NO_REPEAT"));
			noRecord = "TRUE".equals(attrs.getValue("NO_RECORD"));
			noRememberLast = "TRUE".equals(attrs.getValue("NO_REMEMBER_LAST"));
			code.setLength(0);
			isSelected.setLength(0);
		}
	} 

	
	public void endElement(String uri, String localName, String qName)
	{
		String tag = peekElement();

		if (qName.equals(tag))
		{
			if (tag.equals("ACTION"))
			{
				String selected = (isSelected.length() > 0) ?
					isSelected.toString() : null;
				actionSet.addAction(new BeanShellAction(actionName,
					code.toString(),selected,
					noRepeat,noRecord,noRememberLast));
				noRepeat = noRecord = noRememberLast = false;
				code.setLength(0);
				isSelected.setLength(0);
			}

			popElement();
		}
		else
		{
			
			throw new InternalError();
		}
	} 

	
	public void startDocument()
	{
		try
		{
			pushElement(null);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	} 

	

	
	private String path;
	private ActionSet actionSet;

	private String actionName;
	private StringBuffer code;
	private StringBuffer isSelected;

	private boolean noRepeat;
	private boolean noRecord;
	private boolean noRememberLast;

	private Stack stateStack;
	

	
	private String pushElement(String name)
	{
		name = (name == null) ? null : name.intern();

		stateStack.push(name);

		return name;
	} 

	
	private String peekElement()
	{
		return (String) stateStack.peek();
	} 

	
	private String popElement()
	{
		return (String) stateStack.pop();
	} 

	

}
