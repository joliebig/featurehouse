

package org.gjt.sp.jedit;


import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;



class ActionListHandler extends DefaultHandler
{
	
	ActionListHandler(String path, JEditActionSet actionSet)
	{
		this.path = path;
		this.actionSet = actionSet;
		stateStack = new Stack<String>();
		code = new StringBuilder();
		isSelected = new StringBuilder();
	} 

	
	@Override
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

	
	@Override
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

	
	@Override
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

	
	@Override
	public void endElement(String uri, String localName, String qName)
	{
		String tag = peekElement();

		if (qName.equals(tag))
		{
			if (tag.equals("ACTION"))
			{
				String selected = (isSelected.length() > 0) ?
					isSelected.toString() : null;
				JEditAbstractEditAction action = 
					actionSet.createBeanShellAction(actionName,
									code.toString(),
									selected,
									noRepeat,
									noRecord,
									noRememberLast);
				actionSet.addAction(action);
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

	
	@Override
	public void startDocument()
	{
		try
		{
			pushElement(null);
		}
		catch (Exception e)
		{
			Log.log(Log.ERROR,this, e);
		}
	} 

	

	
	private String path;
	private JEditActionSet actionSet;

	private String actionName;
	private final StringBuilder code;
	private final StringBuilder isSelected;

	private boolean noRepeat;
	private boolean noRecord;
	private boolean noRememberLast;

	private final Stack<String> stateStack;
	

	
	private String pushElement(String name)
	{
		name = (name == null) ? null : name.intern();

		stateStack.push(name);

		return name;
	} 

	
	private String peekElement()
	{
		return stateStack.peek();
	} 

	
	private String popElement()
	{
		return stateStack.pop();
	} 

	
}
