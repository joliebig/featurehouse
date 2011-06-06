

package org.gjt.sp.jedit.pluginmgr;


import javax.swing.border.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.ProgressObserver;


class PluginManagerProgress extends JDialog implements ProgressObserver
{
	
	PluginManagerProgress(PluginManager dialog, Roster roster)
	{
		super(dialog,jEdit.getProperty("plugin-manager.progress"),true);

		this.roster = roster;

		JPanel content = new JPanel(new BorderLayout(12,12));
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		progress = new JProgressBar();
		progress.setStringPainted(true);
		progress.setString(jEdit.getProperty("plugin-manager.progress"));

		count = roster.getOperationCount();
		int maximum = 0;
		for(int i = 0; i < count; i++)
		{
			maximum += roster.getOperation(i).getMaximum();
		}

		progress.setMaximum(maximum);
		content.add(BorderLayout.NORTH,progress);

		stop = new JButton(jEdit.getProperty("plugin-manager.progress.stop"));
		stop.addActionListener(new ActionHandler());
		JPanel panel = new JPanel(new FlowLayout(
			FlowLayout.CENTER,0,0));
		panel.add(stop);
		content.add(BorderLayout.CENTER,panel);

		addWindowListener(new WindowHandler());

		pack();
		setLocationRelativeTo(dialog);
		setVisible(true);
	} 

	

	
	@Deprecated
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

	
	
	public void setValue(final long value)
	{
		SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					progress.setValue(valueSoFar + (int) value);
				}
			});
	} 

	
	
	public void setMaximum(long value) 
	{
	} 

	
	
	 public void setStatus(String status) 
	 {
		 setTitle(status);
		 progress.setString(status);
	} 

	
	public void done()
	{
		try
		{
			if(done == count)
			{
				SwingUtilities.invokeAndWait(new Runnable()
				{
					public void run()
					{
						dispose();
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

	

	
	private Thread thread;

	private final JProgressBar progress;
	private final JButton stop;
	private final int count;
	private int done = 1;

	
	private int valueSoFar;

	private final Roster roster;
	

	
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

		@Override
		public void windowOpened(WindowEvent evt)
		{
			if(done)
				return;

			done = true;
			thread = new RosterThread();
			thread.start();
		}

		@Override
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

		@Override
		public void run()
		{
			roster.performOperationsInWorkThread(PluginManagerProgress.this);
		}
	} 

	
}
