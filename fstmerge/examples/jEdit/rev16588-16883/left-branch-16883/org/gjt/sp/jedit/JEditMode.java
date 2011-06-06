

package org.gjt.sp.jedit;

import org.gjt.sp.util.Log;


class JEditMode extends Mode
{
	
	JEditMode(String name)
	{
		super(name);
	} 

	
	
	@Override
	public Object getProperty(String key)
	{
		String prefix = "mode." + name + '.';

		
		
			String property = jEdit.getProperty(prefix + key);
			if(property != null)
			{
				Object value;
				try
				{
					value = new Integer(property);
				}
				catch(NumberFormatException nf)
				{
					value = property;
				}
				return value;
			}
		

		Object value = props.get(key);
		if(value != null)
			return value;

		String global = jEdit.getProperty("buffer." + key);
		if(global != null)
		{
			try
			{
				return new Integer(global);
			}
			catch(NumberFormatException nf)
			{
				return global;
			}
		}
		else
			return null;
	} 

	
	
	@Override
	public void loadIfNecessary()
	{
		if(marker == null)
		{
			jEdit.loadMode(this);
			if (marker == null)
				Log.log(Log.ERROR, this, "Mode not correctly loaded, token marker is still null");
		}
	} 
}
