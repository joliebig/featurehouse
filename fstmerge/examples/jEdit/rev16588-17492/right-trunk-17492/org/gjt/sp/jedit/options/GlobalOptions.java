

package org.gjt.sp.jedit.options;


import java.awt.Dialog;
import java.awt.Frame;
import org.gjt.sp.jedit.gui.OptionsDialog;
import org.gjt.sp.jedit.msg.PropertiesChanging;
import org.gjt.sp.jedit.*;


public class GlobalOptions extends OptionsDialog
{
	
	public GlobalOptions(Frame frame)
	{
		super(frame,"options",jEdit.getProperty("options.last"));
	} 

	
	public GlobalOptions(Frame frame, String pane)
	{
		super(frame,"options",pane);
	} 

	
	public GlobalOptions(Dialog dialog)
	{
		super(dialog,"options",jEdit.getProperty("options.last"));
	} 

	
	public GlobalOptions(Dialog dialog, String pane)
	{
		super(dialog,"options",pane);
	} 

	
	protected OptionTreeModel createOptionTreeModel()
	{
		OptionTreeModel paneTreeModel = new OptionTreeModel();
		OptionGroup rootGroup = (OptionGroup) paneTreeModel.getRoot();

		
		OptionGroup jEditGroup = new OptionGroup("jedit");

		jEditGroup.addOptionPane("general");
		jEditGroup.addOptionPane("abbrevs");
		jEditGroup.addOptionPane("appearance");
		jEditGroup.addOptionPane("context");
		jEditGroup.addOptionPane("docking");
		jEditGroup.addOptionPane("editing");
		jEditGroup.addOptionPane("encodings");
		jEditGroup.addOptionPane("gutter");
		jEditGroup.addOptionPane("mouse");
		jEditGroup.addOptionPane("plugin-manager");
		jEditGroup.addOptionPane("print");
		jEditGroup.addOptionPane("firewall");
		jEditGroup.addOptionPane("save-back");
		jEditGroup.addOptionPane("shortcuts");
		jEditGroup.addOptionPane("status");
		jEditGroup.addOptionPane("syntax");
		jEditGroup.addOptionPane("textarea");
		jEditGroup.addOptionPane("toolbar");
		jEditGroup.addOptionPane("view");
		rootGroup.addOptionGroup(jEditGroup);

		OptionGroup browserGroup = new OptionGroup("browser");
		browserGroup.addOptionPane("browser.general");
		browserGroup.addOptionPane("browser.colors");
		rootGroup.addOptionGroup(browserGroup);

		return paneTreeModel;
	} 

	
	@Override
	public void cancel()
	{
		EditBus.send(
			new PropertiesChanging(null,
				PropertiesChanging.State.CANCELED));
		super.cancel();
	} 

	
	@Override
	protected void init(String name, String pane)
	{
		EditBus.send(
			new PropertiesChanging(null,
				PropertiesChanging.State.LOADING));
		super.init(name, pane);
	} 

	
	protected OptionGroup getDefaultGroup()
	{
		return null;
	} 
}
