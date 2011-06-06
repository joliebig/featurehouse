

package org.gjt.sp.jedit.pluginmgr;


import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.xml.sax.SAXParseException;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.options.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.WorkRequest;



public class PluginManager extends JFrame implements EBComponent
{
	
	
	public static PluginManager getInstance()
	{
		return instance;
	} 

	
	public void dispose()
	{
		instance = null;
		EditBus.removeFromBus(this);
		super.dispose();
	} 

	
	public void handleMessage(EBMessage message)
	{
		if (message instanceof PropertiesChanged)
		{
			if (shouldUpdatePluginList())
			{
				pluginList = null;
				updatePluginList();
				if(tabPane.getSelectedIndex() != 0)
				{
					installer.updateModel();
					updater.updateModel();
				}
			}
		}
		else if (message instanceof PluginUpdate)
		{
			if(!queuedUpdate)
			{
				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						queuedUpdate = false;
						manager.update();
					}
				});
				queuedUpdate = true;
			}
		}
	} 

	
	public static void showPluginManager(Frame parent)
	{
		if (instance == null)
			instance = new PluginManager(parent);
		else
			instance.toFront();
	} 

	
	public void ok()
	{
		dispose();
	} 

	
	public void cancel()
	{
		dispose();
	} 

	
	PluginList getPluginList()
	{
		return pluginList;
	} 

	
	private static PluginManager instance;

	
	private JTabbedPane tabPane;
	private JButton done;
	private JButton mgrOptions;
	private JButton pluginOptions;
	private InstallPanel installer;
	private InstallPanel updater;
	private ManagePanel manager;
	private PluginList pluginList;
	private boolean queuedUpdate;
	private boolean downloadingPluginList;
	private final Frame parent;
	

	
	private PluginManager(Frame parent)
	{
		super(jEdit.getProperty("plugin-manager.title"));
		this.parent = parent;
		init();
	} 

	
	private void init() {
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
		mgrOptions = new JButton(jEdit.getProperty("plugin-manager.mgr-options"));
		mgrOptions.addActionListener(al);
		pluginOptions = new JButton(jEdit.getProperty("plugin-manager.plugin-options"));
		pluginOptions.addActionListener(al);
		done = new JButton(jEdit.getProperty("plugin-manager.done"));
		done.addActionListener(al);

		buttons.add(Box.createGlue());
		buttons.add(mgrOptions);
		buttons.add(Box.createHorizontalStrut(6));
		buttons.add(pluginOptions);
		buttons.add(Box.createHorizontalStrut(6));
		buttons.add(done);
		buttons.add(Box.createGlue());

		getRootPane().setDefaultButton(done);

		content.add(BorderLayout.SOUTH,buttons);

		setDefaultCloseOperation(DISPOSE_ON_CLOSE);

		setIconImage(GUIUtilities.getPluginIcon());

		pack();
		GUIUtilities.loadGeometry(this, parent, "plugin-manager");
		GUIUtilities.addSizeSaver(this, parent, "plugin-manager");
		setVisible(true);
	} 

	
	
	private boolean shouldUpdatePluginList()
	{
		return (pluginList == null ||
			!pluginList.getMirrorId().equals(jEdit.getProperty("plugin-manager.mirror.id"))) &&
			!downloadingPluginList;
	} 

	
	private void updatePluginList()
	{
		if(jEdit.getSettingsDirectory() == null
			&& jEdit.getJEditHome() == null)
		{
			GUIUtilities.error(this,"no-settings",null);
			return;
		}
		if (!shouldUpdatePluginList())
		{
			return;
		}

		final Exception[] exception = new Exception[1];

		VFSManager.runInWorkThread(new WorkRequest()
		{
			public void run()
			{
				try
				{
					downloadingPluginList = true;
					setStatus(jEdit.getProperty(
						"plugin-manager.list-download-connect"));
					pluginList = new PluginList(this);
				}
				catch(Exception e)
				{
					exception[0] = e;
				}
				finally
				{
					downloadingPluginList = false;
				}
			}
		});

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				if(exception[0] instanceof SAXParseException)
				{
					SAXParseException se = (SAXParseException)
						exception[0];

					int line = se.getLineNumber();
					String path = jEdit.getProperty(
						"plugin-manager.export-url");
					String message = se.getMessage();
					Log.log(Log.ERROR,this,path + ':' + line
						+ ": " + message);
					String[] pp = { path,
						String.valueOf(line),
						message };
					GUIUtilities.error(PluginManager.this,
						"plugin-list.xmlerror",pp);
				}
				else if(exception[0] != null)
				{
					Exception e = exception[0];

					Log.log(Log.ERROR,this,e);
					String[] pp = { e.toString() };

					String ok = jEdit.getProperty(
						"common.ok");
					String proxyButton = jEdit.getProperty(
						"plugin-list.ioerror.proxy-servers");
					int retVal =
						JOptionPane.showOptionDialog(
						PluginManager.this,
						jEdit.getProperty("plugin-list.ioerror.message",pp),
						jEdit.getProperty("plugin-list.ioerror.title"),
						JOptionPane.YES_NO_OPTION,
						JOptionPane.ERROR_MESSAGE,
						null,
						new Object[] {
							proxyButton,
							ok
						},
						ok);

					if(retVal == 0)
					{
						new GlobalOptions(
							PluginManager.this,
							"firewall");
					}
				}
			}
		});
	} 

	
	public void processKeyEvents(KeyEvent ke)
	{
		if ((ke.getID() == KeyEvent.KEY_PRESSED) &&
		    (ke.getKeyCode() == KeyEvent.VK_ESCAPE))
		{
			cancel();
			ke.consume();
		}
	} 

	

	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == done)
				ok();
			else if (source == mgrOptions)
				new GlobalOptions(PluginManager.this,"plugin-manager");
			else if (source == pluginOptions)
				new PluginOptions(PluginManager.this);
		}
	} 

	
	class ListUpdater implements ChangeListener
	{
		public void stateChanged(ChangeEvent e)
		{
			Component selected = tabPane.getSelectedComponent();
			if(selected == installer || selected == updater)
			{
				updatePluginList();
				installer.updateModel();
				updater.updateModel();
			}
			else if(selected == manager)
				manager.update();
		}
	} 

	
}
