

package org.gjt.sp.jedit;

import org.gjt.sp.jedit.bsh.*;
import java.lang.ref.SoftReference;
import java.awt.Component;
import org.gjt.sp.jedit.gui.BeanShellErrorDialog;
import org.gjt.sp.util.Log;


public class BeanShellAction extends EditAction
{
	
	public BeanShellAction(String name, String code, String isSelected,
		boolean noRepeat, boolean noRecord, boolean noRememberLast)
	{
		super(name);

		
		String sanitizedName = name.replace('.','_').replace('-','_');
		this.code = new CachedBshMethod("action_" + sanitizedName, code);
		if (isSelected != null)
		{
			this.isSelected = new CachedBshMethod("selected_" + sanitizedName, isSelected);
		}
		this.noRepeat = noRepeat;
		this.noRecord = noRecord;
		this.noRememberLast = noRememberLast;

		jEdit.setTemporaryProperty(name + ".toggle",
			isSelected != null ? "true" : "false");
	} 

	
	public void invoke(View view)
	{
		try
		{
			BeanShell.runCachedBlock(code.get(),view,
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
			View view = GUIUtilities.getView(comp);

			
			
			global.setVariable("_comp",comp);

			return Boolean.TRUE.equals(BeanShell.runCachedBlock(
				isSelected.get(),view,
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
		return code.getSource().trim();
	} 

	
	private boolean noRepeat;
	private boolean noRecord;
	private boolean noRememberLast;
	private CachedBshMethod code;
	private CachedBshMethod isSelected;

	
	private static class CachedBshMethod
	{
		private final String name;
		private final String source;
		private SoftReference<BshMethod> cache;

		public CachedBshMethod(String name, String source)
		{
			this.name = name;
			this.source = source;
			this.cache = null;
		}

		public BshMethod get() throws java.lang.Exception
		{
			if (cache != null)
			{
				BshMethod cached = cache.get();
				if (cached != null)
				{
					return cached;
				}
			}
			BshMethod newOne = BeanShell.cacheBlock(name, source, true);
			cache = new SoftReference<BshMethod>(newOne);
			return newOne;
		}

		public String getSource()
		{
			return source;
		}
	}

	
}
