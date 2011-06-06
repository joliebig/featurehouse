

package org.gjt.sp.jedit.gui;


import javax.swing.border.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.*;


public class IOProgressMonitor extends JPanel
{
	
	public IOProgressMonitor()
	{
		super(new BorderLayout());
		caption = new JLabel();
		updateCaption();
		add(BorderLayout.NORTH,caption);

		threads = new ThreadProgress[VFSManager.getIOThreadPool()
			.getThreadCount()];

		Box box = new Box(BoxLayout.Y_AXIS);
		for(int i = 0; i < threads.length; i++)
		{
			if(i != 0)
				box.add(Box.createVerticalStrut(6));

			threads[i] = new ThreadProgress(i);
			box.add(threads[i]);
		}

		JPanel threadPanel = new JPanel(new BorderLayout());
		threadPanel.setBorder(new EmptyBorder(6,6,6,6));
		threadPanel.add(BorderLayout.NORTH,box);

		add(BorderLayout.CENTER,new JScrollPane(threadPanel));

		workThreadHandler = new WorkThreadHandler();
	} 

	
	public void addNotify()
	{
		VFSManager.getIOThreadPool().addProgressListener(workThreadHandler);
		super.addNotify();
	} 

	
	public void removeNotify()
	{
		VFSManager.getIOThreadPool().removeProgressListener(workThreadHandler);
		super.removeNotify();
	} 

	

	
	private JLabel caption;
	private ThreadProgress[] threads;
	private WorkThreadHandler workThreadHandler;
	

	
	private void updateCaption()
	{
		String[] args = { String.valueOf(VFSManager.getIOThreadPool()
			.getRequestCount()) };
		caption.setText(jEdit.getProperty("io-progress-monitor.caption",args));
	} 

	

	
	class WorkThreadHandler implements WorkThreadProgressListener
	{
		public void statusUpdate(final WorkThreadPool pool, final int index)
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					updateCaption();
					threads[index].update();
				}
			});
		}

		public void progressUpdate(final WorkThreadPool pool, final int index)
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					updateCaption();
					threads[index].update();
				}
			});
		}
	} 

	
	class ThreadProgress extends JPanel
	{
		
		public ThreadProgress(int index)
		{
			super(new BorderLayout(12,12));

			this.index = index;

			Box box = new Box(BoxLayout.Y_AXIS);
			box.add(Box.createGlue());
			box.add(progress = new JProgressBar());
			progress.setStringPainted(true);
			box.add(Box.createGlue());
			ThreadProgress.this.add(BorderLayout.CENTER,box);

			abort = new JButton(jEdit.getProperty("io-progress-monitor.abort"));
			abort.addActionListener(new ActionHandler());
			ThreadProgress.this.add(BorderLayout.EAST,abort);

			update();
		} 

		
		public void update()
		{
			WorkThread thread = VFSManager.getIOThreadPool().getThread(index);
			if(thread.isRequestRunning())
			{
				abort.setEnabled(true);
				String status = thread.getStatus();
				if(status == null)
					status = "";
				progress.setString(status);
				progress.setMaximum(thread.getProgressMaximum());
				
				progress.setValue(thread.getProgressValue());
			}
			else
			{
				abort.setEnabled(false);
				progress.setString(jEdit.getProperty("io-progress-monitor"
					+ ".idle"));
				progress.setValue(0);
			}
		} 

		
		private int index;
		private JProgressBar progress;
		private JButton abort;
		

		
		class ActionHandler implements ActionListener
		{
			public void actionPerformed(ActionEvent evt)
			{
				if(evt.getSource() == abort)
				{
					int result = GUIUtilities.confirm(
						IOProgressMonitor.this,"abort",null,
						JOptionPane.YES_NO_OPTION,
						JOptionPane.QUESTION_MESSAGE);
					if(result == JOptionPane.YES_OPTION)
					{
						VFSManager.getIOThreadPool().getThread(index)
							.abortCurrentRequest();
					}
				}
			}
		} 
	} 
}
