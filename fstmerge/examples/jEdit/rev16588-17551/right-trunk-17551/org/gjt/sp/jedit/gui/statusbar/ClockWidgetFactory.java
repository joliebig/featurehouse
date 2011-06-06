

package org.gjt.sp.jedit.gui.statusbar;


import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.text.DateFormat;
import java.util.Date;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.Timer;
import javax.swing.ToolTipManager;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;



public class ClockWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view) 
	{
		Widget clock = new ClockWidget();
		return clock;
	} 

	
	private static class ClockWidget implements Widget
	{
		private final Clock clock;
		ClockWidget()
		{
			clock = new Clock();
		}
		
		public JComponent getComponent() 
		{
			return clock;
		}
		
		public void update() 
		{
		}
		
		public void propertiesChanged()
		{
		}
	} 

	
	private static class Clock extends JLabel implements ActionListener
	{
		
		Clock()
		{
			setForeground(jEdit.getColorProperty("view.status.foreground"));
			setBackground(jEdit.getColorProperty("view.status.background"));
		} 

		
		@Override
		public void addNotify()
		{
			super.addNotify();
			update();

			int millisecondsPerMinute = 1000 * 60;

			timer = new Timer(millisecondsPerMinute,this);
			timer.setInitialDelay((int)(
				millisecondsPerMinute
				- System.currentTimeMillis()
				% millisecondsPerMinute) + 500);
			timer.start();
			ToolTipManager.sharedInstance().registerComponent(this);
		} 

		
		@Override
		public void removeNotify()
		{
			timer.stop();
			ToolTipManager.sharedInstance().unregisterComponent(this);
			super.removeNotify();
		} 

		
		@Override
		public String getToolTipText()
		{
			return new Date().toString();
		} 

		
		@Override
		public Point getToolTipLocation(MouseEvent event)
		{
			return new Point(event.getX(),-20);
		} 

		
		public void actionPerformed(ActionEvent e)
		{
			update();
		} 

		
		private Timer timer;

		
		private static String getTime()
		{
			return DateFormat.getTimeInstance(
				DateFormat.SHORT).format(new Date());
		} 

		
		private void update()
		{
			setText(getTime());
		} 

		
	} 
}
