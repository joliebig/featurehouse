

package org.gjt.sp.jedit.options;


import java.awt.Dialog;
import java.awt.Frame;
import org.gjt.sp.jedit.gui.OptionsDialog;
import org.gjt.sp.jedit.options.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;


public class PluginOptions extends OptionsDialog
{
	
	public PluginOptions(Frame frame)
	{
		super(frame,"plugin-options",jEdit.getProperty("plugin-options.last"));
	} 

	
	public PluginOptions(Frame frame, String pane)
	{
		super(frame,"plugin-options",pane);
	} 

	
	public PluginOptions(Dialog dialog)
	{
		super(dialog,"plugin-options",jEdit.getProperty("plugin-options.last"));
	} 

	
	public PluginOptions(Dialog dialog, String pane)
	{
		super(dialog,"plugin-options",pane);
	} 

	
	protected OptionTreeModel createOptionTreeModel()
	{
		OptionTreeModel paneTreeModel = new OptionTreeModel();
		OptionGroup rootGroup = (OptionGroup) paneTreeModel.getRoot();

		
		pluginsGroup = new OptionGroup("plugins");
		pluginsGroup.setSort(true);

		
		EditPlugin[] plugins = jEdit.getPlugins();
		for(int i = 0; i < plugins.length; i++)
		{
			EditPlugin ep = plugins[i];
			if(ep instanceof EditPlugin.Broken)
				continue;

			String className = ep.getClassName();
			if(jEdit.getProperty("plugin." + className + ".activate") == null)
			{
				
				try
				{
					ep.createOptionPanes(this);
				}
				catch(Throwable t)
				{
					Log.log(Log.ERROR, ep,
						"Error creating option pane");
					Log.log(Log.ERROR, ep, t);
				}
			}
			else
			{
				String optionPane = jEdit.getProperty(
					"plugin." + className + ".option-pane");
				if(optionPane != null)
					pluginsGroup.addOptionPane(optionPane);
				else
				{
					String options = jEdit.getProperty(
						"plugin." + className
						+ ".option-group");
					if(options != null)
					{
						pluginsGroup.addOptionGroup(
							new OptionGroup(
							"plugin." + className,
							jEdit.getProperty("plugin."
							+ className + ".name"),
							options)
						);
					}
				}
			}
		}

		
		if (pluginsGroup.getMemberCount() == 0)
			pluginsGroup.addOptionPane(new NoPluginsPane());

		rootGroup.addOptionGroup(pluginsGroup);

		return paneTreeModel;
	} 

	
	protected OptionGroup getDefaultGroup()
	{
		return pluginsGroup;
	} 

	
	private OptionGroup pluginsGroup;
	

	
	static class NoPluginsPane extends AbstractOptionPane
	{
		public NoPluginsPane()
		{
			super("no-plugins");
		}
	} 
}
