

package org.gjt.sp.jedit.gui;


import java.awt.*;
import java.awt.event.*;
import javax.swing.*;



public class AnimatedIcon extends ImageIcon
{
	
	
	public AnimatedIcon(Image icon, Image[] frames, int rate, Component host)
	{
		super(icon);
		this.icon = icon;
		this.frames = frames;
		delay = 1000/rate;
		this.host = host;
	} 

	
	public Image[] getFrames()
	{
		return frames;
	} 

	
	public Image getIcon()
	{
		return icon;
	} 

	
	public int getRate()
	{
		return 1000/delay;
	} 

	
	public void setFrames(Image[] frames)
	{
		this.frames = frames;
	} 

	
	public void setIcon(Image icon)
	{
		this.icon = icon;
	} 

	
	public void setRate(int rate)
	{
		delay = 1000/rate;
	} 

	
	
	public void start()
	{
		if(timer != null)
			return;

		timer = new Timer(delay,new Animator());
		timer.start();
	} 

	
	
	public void stop()
	{
		current = 0;
		if(timer != null)
		{
			timer.stop();
			timer = null;
		}

		setImage(icon);
		host.repaint();
	} 

	
	private Image[] frames;
	private int current;
	private int delay;
	private Timer timer;
	private Component host;
	private Image icon;
	

	
	class Animator implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			current = (current + 1) % frames.length;
			setImage(frames[current]);
			host.repaint();
		}
	} 
}
