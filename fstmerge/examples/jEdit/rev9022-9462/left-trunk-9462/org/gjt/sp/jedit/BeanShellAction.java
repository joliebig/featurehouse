

package org.gjt.sp.jedit;

import bsh.*;
import java.awt.Component;
import org.gjt.sp.jedit.gui.BeanShellErrorDialog;
import org.gjt.sp.util.Log;


public class BeanShellAction extends EditAction
{
	
	public BeanShellAction(String name, String code, String isSelected,
		boolean noRepeat, boolean noRecord, boolean noRememberLast)
	{
		super(name);

		this.code = code;
		this.isSelected = isSelected;
		this.noRepeat = noRepeat;
		this.noRecord = noRecord;
		this.noRememberLast = noRememberLast;

		
		sanitizedName = name.replace('.','_').replace('-','_');

		jEdit.setTemporaryProperty(name + ".toggle",
			isSelected != null ? "true" : "false");
	} 

	
	public void invoke(View view)
	{
		try
		{
			if(cachedCode == null)
			{
				String cachedCodeName = "action_" + sanitizedName;
				cachedCode = BeanShell.cacheBlock(cachedCodeName,code,true);
			}

			BeanShell.runCachedBlock(cachedCode,view,
				new NameSpace(BeanShell.getNameSpace(),
				"BeanShellAction.invoke()"));
		}
		catch(Throwable e)
		{
			Log.log(Log.ERROR,this,e);

			new BeanShellErrorDialog(view,e);
		}
	} 

	
	public boolean isSelected(Component comp)
	{
		if(isSelected == null)
			return false;

		NameSpace global = BeanShell.getNameSpace();

		try
		{
			if(cachedIsSelected == null)
			{
				String cachedIsSelectedName = "selected_" + sanitizedName;
				cachedIsSelected = BeanShell.cacheBlock(cachedIsSelectedName,
					isSelected,true);
			}

			View view = GUIUtilities.getView(comp);

			
			
			global.setVariable("_comp",comp);

			return Boolean.TRUE.equals(BeanShell.runCachedBlock(
				cachedIsSelected,view,
				new NameSpace(BeanShell.getNameSpace(),
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
	
}
