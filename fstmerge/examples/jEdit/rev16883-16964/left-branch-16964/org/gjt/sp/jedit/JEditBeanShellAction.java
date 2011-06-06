

package org.gjt.sp.jedit;

import org.gjt.sp.jedit.bsh.*;
import java.awt.Component;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.util.Log;


public class JEditBeanShellAction extends JEditAbstractEditAction<TextArea>
{
	
	public JEditBeanShellAction(String name, String code, String isSelected,
		boolean noRepeat, boolean noRecord, boolean noRememberLast)
	{
		super(name);

		this.code = code;
		this.isSelected = isSelected;
		this.noRepeat = noRepeat;
		this.noRecord = noRecord;
		this.noRememberLast = noRememberLast;

		
		sanitizedName = name.replace('.','_').replace('-','_');
	} 

	
	public void invoke(TextArea textArea)
	{
		try
		{
			if(cachedCode == null)
			{
				String cachedCodeName = "action_" + sanitizedName;
				cachedCode = bsh.cacheBlock(cachedCodeName,code,true);
			}

			bsh.runCachedBlock(cachedCode,textArea,
				new NameSpace(bsh.getNameSpace(),
				"BeanShellAction.invoke()"));
		}
		catch(Throwable e)
		{
			Log.log(Log.ERROR,this,e);
		}
	} 

	
	public boolean isSelected(Component comp)
	{
		if(isSelected == null)
			return false;

		NameSpace global = bsh.getNameSpace();

		try
		{
			if(cachedIsSelected == null)
			{
				String cachedIsSelectedName = "selected_" + sanitizedName;
				cachedIsSelected = bsh.cacheBlock(cachedIsSelectedName,
					isSelected,true);
			}

			
			
			global.setVariable("_comp",comp);

			return Boolean.TRUE.equals(bsh.runCachedBlock(
				cachedIsSelected,null,
				new NameSpace(bsh.getNameSpace(),
				"BeanShellAction.isSelected()")));
		}
		catch(Throwable e)
		{
			Log.log(Log.ERROR,this,e);

			
			

			
			
			isSelected = null;

			return false;
		}
		finally
		{
			try
			{
				global.setVariable("_comp",null);
			}
			catch(UtilEvalError err)
			{
				Log.log(Log.ERROR,this,err);
			}
		}
	} 

	
	public boolean noRepeat()
	{
		return noRepeat;
	} 

	
	public boolean noRecord()
	{
		return noRecord;
	} 

	
	
	public boolean noRememberLast()
	{
		return noRememberLast;
	} 

	
	public String getCode()
	{
		return code.trim();
	} 

	
	private boolean noRepeat;
	private boolean noRecord;
	private boolean noRememberLast;
	private String code;
	private String isSelected;
	private BshMethod cachedCode;
	private BshMethod cachedIsSelected;
	private String sanitizedName;
	private static final BeanShellFacade<TextArea> bsh = new MyBeanShellFacade();
	
	
	
	private static class MyBeanShellFacade extends BeanShellFacade<TextArea>
	{
		@Override
		protected void setupDefaultVariables(NameSpace namespace, TextArea textArea) throws UtilEvalError 
		{
			if(textArea != null)
			{
				namespace.setVariable("buffer",textArea.getBuffer(), false);
				namespace.setVariable("textArea",textArea, false);
			}
		}

		@Override
		protected void resetDefaultVariables(NameSpace namespace) throws UtilEvalError
		{
			namespace.setVariable("buffer",null, false);
			namespace.setVariable("textArea",null, false);
		}

		@Override
		protected void handleException(TextArea textArea, String path, Throwable t)
		{
			Log.log(Log.ERROR,this, t, t);

		}
	} 

}
