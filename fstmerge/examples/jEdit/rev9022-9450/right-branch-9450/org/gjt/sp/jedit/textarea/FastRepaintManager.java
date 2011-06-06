

package org.gjt.sp.jedit.textarea;


import java.awt.*;



class FastRepaintManager
{
	
	FastRepaintManager(TextArea textArea,
		TextAreaPainter painter)
	{
		this.textArea = textArea;
		this.painter = painter;
	} 

	
	void updateGraphics()
	{
		if(gfx != null)
			gfx.dispose();

		int width = painter.getWidth();
		int height = painter.getHeight();
		
		if(width <= 0)
			width = 1;
		if(height <= 0)
			height = 1;
		img = painter.getGraphicsConfiguration()
			.createCompatibleImage(width,height,
			Transparency.OPAQUE);
		gfx = (Graphics2D)img.getGraphics();
		gfx.clipRect(0,0,painter.getWidth(),painter.getHeight());
		fastScroll = false;
	} 

	
	Graphics2D getGraphics()
	{
		return gfx;
	} 

	
	static class RepaintLines
	{
		final int first;
		final int last;

		RepaintLines(int first, int last)
		{
			this.first = first;
			this.last = last;
		}
	} 

	
	RepaintLines prepareGraphics(Rectangle clipRect, int firstLine,
		Graphics2D gfx)
	{
		gfx.setFont(painter.getFont());
		gfx.setColor(painter.getBackground());

		int height = gfx.getFontMetrics().getHeight();

		if(fastScroll)
		{
			int lineDelta = this.firstLine - firstLine;
			int yDelta = lineDelta * height;
			int visibleLines = textArea.getVisibleLines();

			if(lineDelta > -visibleLines
				&& lineDelta < visibleLines)
			{
				if(lineDelta < 0)
				{
					gfx.copyArea(0,-yDelta,painter.getWidth(),
						painter.getHeight() + yDelta,0,yDelta);
					return new RepaintLines(
						visibleLines + this.firstLine
						- firstLine - 1,
						visibleLines - 1);
				}
				else if(lineDelta > 0)
				{
					gfx.copyArea(0,0,painter.getWidth(),
						painter.getHeight() - yDelta,0,yDelta);
					return new RepaintLines(0,
						this.firstLine - firstLine);
				}
			}
		}

		
		
		
		return new RepaintLines(
			clipRect.y / height,
			(clipRect.y + clipRect.height - 1) / height);
	} 

	
	void paint(Graphics g)
	{
		firstLine = textArea.getFirstLine();
		g.drawImage(img,0,0,null);
	} 

	
	void setFastScroll(boolean fastScroll)
	{
		this.fastScroll = fastScroll;
	} 

	
	private TextArea textArea;
	private TextAreaPainter painter;
	private Graphics2D gfx;
	private Image img;
	private boolean fastScroll;
	
	private int firstLine;
	
}
