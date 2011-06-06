

package macos;


import java.awt.*;
import javax.swing.*;
import org.gjt.sp.jedit.*;


public class MacOSOptionPane extends AbstractOptionPane
{
	
	private JCheckBox menuBox;
	private JCheckBox preserveBox;
	private JCheckBox selectionBox;
	private JCheckBox setTypeCreatorBox;
	

	
	public MacOSOptionPane()
	{
		super("MacOSPlugin");
	}

	
	public void _init()
	{
		Dimension d = new Dimension(7,7);
		Dimension d_2 = new Dimension(20,20);

		menuBox = new JCheckBox(jEdit.getProperty("options.MacOSPlugin.menubar.label"));
		addComponent(menuBox);
		addComponent(new JLabel("(Requires restart for changes to take effect)"));

		addComponent(new Box.Filler(d,d,d));

		preserveBox = new JCheckBox(jEdit.getProperty("options.MacOSPlugin.preserve.label"));
		addComponent(preserveBox);

		addComponent(new Box.Filler(d,d,d));

		selectionBox = new JCheckBox(jEdit.getProperty("options.MacOSPlugin.useSelection.label"));
		addComponent(selectionBox);

		setTypeCreatorBox = new JCheckBox(jEdit.getProperty("options.MacOSPlugin.setTypeCreator.label"));
		addComponent(setTypeCreatorBox);

		getSettings();
	}

	
	public void _save()
	{
		jEdit.setBooleanProperty("MacOSPlugin.useScreenMenuBar", menuBox.isSelected());
		jEdit.setBooleanProperty("MacOSPlugin.preserveCodes", preserveBox.isSelected());
		jEdit.setBooleanProperty("MacOSPlugin.useSelection", selectionBox.isSelected());
		jEdit.setBooleanProperty("MacOSPlugin.setTypeCreator", setTypeCreatorBox.isSelected());
	}

	
	public void getSettings()
	{
		menuBox.setSelected(jEdit.getBooleanProperty("MacOSPlugin.useScreenMenuBar",
			jEdit.getBooleanProperty("MacOSPlugin.default.useScreenMenuBar")));
		preserveBox.setSelected(jEdit.getBooleanProperty("MacOSPlugin.preserveCodes",
			jEdit.getBooleanProperty("MacOSPlugin.default.preserveCodes")));
		selectionBox.setSelected(jEdit.getBooleanProperty("MacOSPlugin.useSelection",
			jEdit.getBooleanProperty("MacOSPlugin.default.useSelection")));
		setTypeCreatorBox.setSelected(jEdit.getBooleanProperty("MacOSPlugin.setTypeCreator",
			jEdit.getBooleanProperty("MacOSPlugin.default.setTypeCreator")));
	}
}
