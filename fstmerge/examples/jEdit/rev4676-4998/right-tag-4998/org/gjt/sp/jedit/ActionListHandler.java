

package org.gjt.sp.jedit;


import com.microstar.xml.*;
import java.io.*;
import java.util.Stack;
import org.gjt.sp.util.Log;


class ActionListHandler extends HandlerBase
{
	
	ActionListHandler(String path, ActionSet actionSet)
	{
		this.path = path;
		this.actionSet = actionSet;
		stateStack = new Stack();
	} 

	
	public Object resolveEntity(String publicId, String systemId)
	{
		if("actions.dtd".equals(systemId))
		{
			
			
			
			return new StringReader("<!-- -->");

			
		}

		return null;
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

	
	public void doctypeDecl(String name, String publicId,
		String systemId) throws Exception
	{
		if("ACTIONS".equals(name))
			return;

		Log.log(Log.ERROR,this,path + ": DOCTYPE must be ACTIONS");
	} 

	
	public void charData(char[] c, int off, int len)
	{
		String tag = peekElement();
		String text = new String(c, off, len);

		if (tag == "CODE")
		{
			code = text;
		}
		else if (tag == "IS_SELECTED")
		{
			isSelected = text;
		}
	} 

	
	public void startElement(String tag)
	{
		tag = pushElement(tag);

		if (tag == "ACTION")
		{
			code = null;
			isSelected = null;
		}
	} 

	
	public void endElement(String name)
	{
		if(name == null)
			return;

		String tag = peekElement();

		if(name.equals(tag))
		{
			if(tag == "ACTION")
			{
				actionSet.addAction(new BeanShellAction(actionName,
					code,isSelected,noRepeat,noRecord,
					noRememberLast));
				noRepeat = noRecord = noRememberLast = false;
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
	private String code;
	private String isSelected;

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
