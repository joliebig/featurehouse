

package org.gjt.sp.jedit.pluginmgr;


import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.options.GlobalOptions;
import org.gjt.sp.jedit.*;


public class PluginManager extends JFrame implements EBComponent
{
	
	public PluginManager(Frame frame)
	{
		super(jEdit.getProperty("plugin-manager.title"));

		EditBus.addToBus(this);

		
		JPanel content = new JPanel(new BorderLayout(12,12));
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		tabPane = new JTabbedPane();
		tabPane.addTab(jEdit.getProperty("manage-plugins.title"),
			manager = new ManagePanel(this));
		tabPane.addTab(jEdit.getProperty("update-plugins.title"),
			updater = new InstallPanel(this,true));
		tabPane.addTab(jEdit.getProperty("install-plugins.title"),
			installer = new InstallPanel(this,false));

		content.add(BorderLayout.CENTER,tabPane);

		tabPane.addChangeListener(new ListUpdater());

		
		Box buttons = new Box(BoxLayout.X_AXIS);

		ActionListener al = new ActionHandler();
		options = new JButton(jEdit.getProperty("plugin-manager.options"));
		options.addActionListener(al);
		done = new JButton(jEdit.getProperty("plugin-manager.done"));
		done.addActionListener(al);

		buttons.add(Box.createGlue());
		buttons.add(options);
		buttons.add(Box.createHorizontalStrut(6));
		buttons.add(done);
		buttons.add(Box.createGlue());

		getRootPane().setDefaultButton(done);

		content.add(BorderLayout.SOUTH,buttons);

		setDefaultCloseOperation(DISPOSE_ON_CLOSE);

		pack();
		GUIUtilities.loadGeometry(this,"plugin-manager");
		show();
	} 

	
	public void dispose()
	{
		GUIUtilities.saveGeometry(this,"plugin-manager");
		instance = null;
		EditBus.removeFromBus(this);
		super.dispose();
	} 

	
	public void handleMessage(EBMessage message)
	{
		
		
		
	} 

	
	public static void showPluginManager(Frame frame)
	{
		if (instance == null)
			instance = new PluginManager(frame);
		else
		{
			instance.toFront();
			return;
		}
	} 

	
	public void ok()
	{
		dispose();
	} 

	
	public void cancel()
	{
		dispose();
	} 

	
	public PluginList getPluginList()
	{
		return pluginList;
	} 

	

	
	private JTabbedPane tabPane;
	private JButton done;
	private JButton cancel;
	private JButton options;
	private InstallPanel installer;
	private InstallPanel updater;
	private ManagePanel manager;
	private PluginList pluginList;

	private static PluginManager instance;
	

	
	private void updatePluginList()
	{
		if(jEdit.getSettingsDirectory() == null
			&& jEdit.getJEditHome() == null)
		{
			GUIUtilities.error(this,"no-settings",null);
		}
		else if(pluginList == null)
		{
			pluginList = new PluginListDownloadProgress(this)
				.getPluginList();
		}
	} 

	

	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == done)
				ok();
			else if (source == cancel)
				cancel();
			else if (source == options)
				new GlobalOptions(PluginManager.this,"plugin-manager");
		}
	} 

	
	class ListUpdater implements ChangeListener
	{
		public void stateChanged(ChangeEvent e)
		{
			final Component selected = tabPane.getSelectedComponent();
			if(selected == installer || selected == updater)
			{
				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						updatePluginList();
						installer.updateModel();
						updater.updateModel();
					}
				});
			}
			else if(selected == manager)
				manager.update();
		}
	} 

	
}
