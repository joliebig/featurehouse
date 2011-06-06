

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import java.awt.*;

import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.util.Log;



public class SplashScreen extends JComponent
{
	
	public SplashScreen()
	{
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		setBackground(Color.white);

		setFont(defaultFont);
		fm = getFontMetrics(defaultFont);
		image = getToolkit().getImage(
			getClass().getResource("/org/gjt/sp/jedit/icons/splash.png"));
		MediaTracker tracker = new MediaTracker(this);
		tracker.addImage(image,0);

		try
		{
			tracker.waitForAll();
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,e);
		}
		Dimension screen = getToolkit().getScreenSize(); 
		win = new JWindow();
		GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
		GraphicsDevice[] gs = ge.getScreenDevices();
		GraphicsDevice gd = gs[0];
		if (gd != null)
		{
			GraphicsConfiguration gconf = gd.getDefaultConfiguration();
			if (gconf != null)
			{
				Rectangle bounds = gconf.getBounds();
				screen = new Dimension(bounds.width, bounds.height);
			}
		}
		Dimension size = new Dimension(image.getWidth(this) + 2,
			image.getHeight(this) + 2 + PROGRESS_HEIGHT);
		win.setSize(size);

		win.getContentPane().add(this, BorderLayout.CENTER);

		win.setLocation((screen.width - size.width) / 2,
			(screen.height - size.height) / 2);
		win.validate();
		win.setVisible(true);
	} 

	
	public void dispose()
	{
		win.dispose();
	} 

	
	public synchronized void advance()
	{
		logAdvanceTime(null);
		progress++;
		repaint();

		
		
		try
		{
			wait();
		}
		catch(InterruptedException ie)
		{
			Log.log(Log.ERROR,this,ie);
		}
	}

	public synchronized void advance(String label)
	{
		logAdvanceTime(label);
		progress++;
		this.label = label;
		repaint();

		
		
		try
		{
			wait();
		}
		catch(InterruptedException ie)
		{
			Log.log(Log.ERROR,this,ie);
		}
	} 

	
	private void logAdvanceTime(String label)
	{
		long currentTime = System.currentTimeMillis();
		if (lastLabel != null)
		{
			Log.log(Log.DEBUG, SplashScreen.class,
				lastLabel +':'+(currentTime - lastAdvanceTime) + "ms");
		}
		if (label != null)
		{
			lastLabel = label;
			lastAdvanceTime = currentTime;

		}
	} 

	
	@Override
	public synchronized void paintComponent(Graphics g)
	{
		Dimension size = getSize();

		g.setColor(Color.black);
		g.drawRect(0,0,size.width - 1,size.height - 1);

		g.drawImage(image,1,1,this);

		
		g.setColor(Color.white);
		g.fillRect(1,image.getHeight(this) + 1,
			((win.getWidth() - 2) * progress) / PROGRESS_COUNT, PROGRESS_HEIGHT);

		g.setColor(Color.black);

		if (label != null)
		{
			int drawOffsetX = (getWidth() - fm.stringWidth(label)) / 2;
			int drawOffsetY = image.getHeight(this) + (PROGRESS_HEIGHT
							      + fm.getAscent() + fm.getDescent()) / 2;

			paintString(g, label, drawOffsetX, drawOffsetY);
		}

		String version = "version " + jEdit.getVersion();

		int drawOffsetX = (getWidth() / 2) - (fm.stringWidth(version) / 2);
		int drawOffsetY = image.getHeight(this) - fm.getDescent() - 2;

		paintString(g, version, drawOffsetX, drawOffsetY);

		notify();
	} 

	
	private void paintString(Graphics g, String version, int drawOffsetX,
				 int drawOffsetY)
	{
		g.setFont( labelFont );

		g.setColor( versionColor1 );
		g.drawString( version, drawOffsetX, drawOffsetY );
		
		g.setColor( versionColor2 );
		g.drawString( version, drawOffsetX + 1, drawOffsetY + 1 );
	} 

	
	private final FontMetrics fm;
	private final JWindow win;
	private final Image image;
	private int progress;
	private static final int PROGRESS_HEIGHT = 20;
	private static final int PROGRESS_COUNT = 28;
	private String label;
	private String lastLabel;
	private long lastAdvanceTime = System.currentTimeMillis();
	private Font defaultFont = new Font("Dialog",Font.PLAIN,10);
	private Font labelFont = UIManager.getFont("Label.font").deriveFont(9.8f);
	private Color versionColor1 = new Color(55, 55, 55);
	private Color versionColor2 = new Color(255, 255, 255, 50);
	
}
