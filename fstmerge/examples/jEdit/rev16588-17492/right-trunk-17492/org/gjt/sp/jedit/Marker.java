

package org.gjt.sp.jedit;

import javax.swing.text.Position;


public class Marker
{
	
	
	public char getShortcut()
	{
		return shortcut;
	} 

	
	
	public int getPosition()
	{
		return (position == null ? pos : position.getOffset());
	} 

	

	
	Marker(Buffer buffer, char shortcut, int position)
	{
		this.buffer = buffer;
		this.shortcut = shortcut;
		this.pos = position;
	} 

	
	
	void setShortcut(char shortcut)
	{
		this.shortcut = shortcut;
	} 

	
	void createPosition()
	{
		position = buffer.createPosition(pos);
	} 

	
	void removePosition()
	{
		
		if(position != null)
		{
			pos = position.getOffset();
			position = null;
		}
	} 

	
	
	void setPosition(int pos)
	{
		this.pos = pos;
	} 

	

	
	private Buffer buffer;
	private char shortcut;
	private int pos;
	private Position position;
	
}
