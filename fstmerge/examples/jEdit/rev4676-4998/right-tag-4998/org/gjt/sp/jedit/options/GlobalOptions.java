

package org.gjt.sp.jedit.options;


import java.awt.Dialog;
import java.awt.Frame;
import org.gjt.sp.jedit.gui.OptionsDialog;
import org.gjt.sp.jedit.options.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;


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

		
		jEditGroup = new OptionGroup("jedit");

		jEditGroup.addOptionPane("context");
		jEditGroup.addOptionPane("editing");
		jEditGroup.addOptionPane("general");
		jEditGroup.addOptionPane("gutter");
		jEditGroup.addOptionPane("loadsave");
		jEditGroup.addOptionPane("print");
		jEditGroup.addOptionPane("plugin-manager");
		jEditGroup.addOptionPane("firewall");
		jEditGroup.addOptionPane("shortcuts");
		jEditGroup.addOptionPane("status");
		jEditGroup.addOptionPane("syntax");
		jEditGroup.addOptionPane("textarea");
		jEditGroup.addOptionPane("toolbar");
		rootGroup.addOptionGroup(jEditGroup);

		browserGroup = new OptionGroup("browser");
		browserGroup.addOptionPane("browser.general");
		browserGroup.addOptionPane("browser.colors");
		rootGroup.addOptionGroup(browserGroup);

		return paneTreeModel;
	} 

	
	protected OptionGroup getDefaultGroup()
	{
		return null;
	} 

	
	private OptionGroup jEditGroup;
	private OptionGroup browserGroup;
	
}
