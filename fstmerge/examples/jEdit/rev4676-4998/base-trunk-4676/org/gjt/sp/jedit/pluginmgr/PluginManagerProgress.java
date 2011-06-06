

package org.gjt.sp.jedit.pluginmgr;


import javax.swing.border.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.*;


class PluginManagerProgress extends JDialog
{
	
	public PluginManagerProgress(PluginManager dialog, String type, Roster roster)
	{
		super(dialog,
			jEdit.getProperty("plugin-manager.progress."
			+ type + "-task"),true);

		this.dialog = dialog;
		this.roster = roster;
		this.type = type;

		JPanel content = new JPanel(new BorderLayout(12,12));
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		progress = new JProgressBar();
		progress.setStringPainted(true);
		progress.setString(jEdit.getProperty("plugin-manager.progress."
			+ type + "-task"));

		int maximum = 0;
		count = roster.getOperationCount();
		for(int i = 0; i < count; i++)
		{
			maximum += roster.getOperation(i).getMaximum();
		}

		progress.setMaximum(maximum);
		content.add(BorderLayout.CENTER,progress);

		stop = new JButton(jEdit.getProperty("plugin-manager.progress.stop"));
		stop.addActionListener(new ActionHandler());
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel,BoxLayout.X_AXIS));
		panel.add(Box.createGlue());
		panel.add(stop);
		panel.add(Box.createGlue());
		content.add(BorderLayout.SOUTH,panel);

		addWindowListener(new WindowHandler());

		pack();

		Dimension size = getSize();
		size.width = Math.max(size.width,500);
		setSize(size);
		setLocationRelativeTo(dialog);

		show();
	} 

	
	public void removing(String plugin)
	{
		String[] args = { plugin };
		showMessage(jEdit.getProperty("plugin-manager.progress.removing",args));
		stop.setEnabled(true);
	} 

	
	public void downloading(String plugin)
	{
		String[] args = { plugin };
		showMessage(jEdit.getProperty("plugin-manager.progress.downloading",args));
		stop.setEnabled(true);
	} 

	
	public void installing(String plugin)
	{
		String[] args = { plugin };
		showMessage(jEdit.getProperty("plugin-manager.progress.installing",args));
		stop.setEnabled(false);
	} 

	
	public void setValue(final int value)
	{
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				progress.setValue(valueSoFar + value);
			}
		});
	} 

	
	public void done(final boolean ok)
	{
		this.ok |= ok;

		try
		{
			if(!ok || done == count)
			{
				SwingUtilities.invokeAndWait(new Runnable()
				{
					public void run()
					{
						dispose();
						if(ok)
						{
							GUIUtilities.message(dialog,
								"plugin-manager." + type
								+ "-done",null);
						}
						else
						{
							

							
							
						}
					}
				});
			}
			else
			{
				SwingUtilities.invokeAndWait(new Runnable()
				{
					public void run()
					{
						valueSoFar += roster.getOperation(done - 1)
							.getMaximum();
						progress.setValue(valueSoFar);
						done++;
					}
				});
			}
		}
		catch(Exception e)
		{
		}
	} 

	
	public boolean isOK()
	{
		return ok;
	} 

	

	
	private PluginManager dialog;

	private Thread thread;

	private String type;

	private JProgressBar progress;
	private JButton stop;
	private int count;
	private int done = 1;

	
	private int valueSoFar;

	private boolean ok;

	private Roster roster;
	

	
	private void showMessage(final String msg)
	{
		
	} 

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			if(evt.getSource() == stop)
			{
				thread.stop();
				dispose();
			}
		}
	} 

	
	class WindowHandler extends WindowAdapter
	{
		boolean done;

		public void windowOpened(WindowEvent evt)
		{
			if(done)
				return;

			done = true;
			thread = new RosterThread();
			thread.start();
		}

		public void windowClosing(WindowEvent evt)
		{
			thread.stop();
			dispose();
		}
	} 

	
	class RosterThread extends Thread
	{
		RosterThread()
		{
			super("Plugin manager thread");
		}

		public void run()
		{
			roster.performOperations(PluginManagerProgress.this);
		}
	} 
	
	
}
