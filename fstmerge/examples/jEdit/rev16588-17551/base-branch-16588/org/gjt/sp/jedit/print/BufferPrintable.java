

package org.gjt.sp.jedit.print;


import javax.swing.text.TabExpander;
import javax.swing.SwingUtilities;
import java.awt.font.*;
import java.awt.geom.*;
import java.awt.print.*;
import java.awt.*;
import java.lang.reflect.Method;
import java.util.*;
import java.util.List;

import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.*;



class BufferPrintable implements Printable
{
	
	BufferPrintable(PrinterJob job, Object format,
		View view, Buffer buffer, Font font, boolean header,
		boolean footer, boolean lineNumbers, boolean color)
	{
		this.job = job;
		this.format = format;
		this.view = view;
		this.buffer = buffer;
		this.font = font;
		this.header = header;
		this.footer = footer;
		this.lineNumbers = lineNumbers;

		styles = org.gjt.sp.util.SyntaxUtilities.loadStyles(jEdit.getProperty("print.font"),
			jEdit.getIntegerProperty("print.fontsize",10),color);
		styles[Token.NULL] = new SyntaxStyle(textColor,null,font);

		
		for(int i = 0; i < styles.length; i++)
		{
			SyntaxStyle s = styles[i];
			if(s.getForegroundColor().equals(Color.WHITE)
				&& s.getBackgroundColor() == null)
			{
				styles[i] = new SyntaxStyle(
					Color.BLACK,
					styles[i].getBackgroundColor(),
					styles[i].getFont());
			}
		}

		lineList = new ArrayList<Chunk>();

		tokenHandler = new DisplayTokenHandler();
	} 

	
	public void print()
	{
		try
		{
			

			if(format == null)
				job.print();
			else
			{
				Method method = PrinterJob.class.getMethod(
					"print",new Class[] { Class.forName(
					"javax.print.attribute.PrintRequestAttributeSet") });
				method.invoke(job,new Object[] { format });
			}
		}
		catch(PrinterAbortException ae)
		{
			Log.log(Log.DEBUG,this,ae);
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,e);
			final String[] args = { e.toString() };
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					GUIUtilities.error(view,"print-error",args);
				}
			});
		}
		finally
		{
			
		}
	} 

	
	public int print(Graphics _gfx, PageFormat pageFormat, int pageIndex)
		throws PrinterException
	{
		
		
		if(frc == null)
		{
			frc = ((Graphics2D)_gfx).getFontRenderContext();
			Log.log(Log.DEBUG,this,"Font render context is " + frc);
		}

		Log.log(Log.DEBUG,this,"Asked to print page " + pageIndex);
		Log.log(Log.DEBUG,this,"Current page is " + currentPage);

		if(pageIndex > currentPage)
		{
			for(int i = currentPage; i < pageIndex; i++)
			{
				Log.log(Log.DEBUG,this,"Current physical line is now " + currentPageStart);
				currentPhysicalLine = currentPageStart;
				printPage(_gfx,pageFormat,i,true);
			}

			currentPage = pageIndex - 1;
			Log.log(Log.DEBUG,this,"Current page is now " + currentPage);
		}

		if(pageIndex == currentPage + 1)
		{
			if(end)
			{
				Log.log(Log.DEBUG,this,"The end");
				return NO_SUCH_PAGE;
			}

			currentPageStart = currentPhysicalLine;
			Log.log(Log.DEBUG,this,"#2 - Current physical line is now " + currentPageStart);
			currentPage = pageIndex;
			Log.log(Log.DEBUG,this,"#2 - Current page is now " + currentPage);
		}
		else if(pageIndex == currentPage)
		{
			currentPhysicalLine = currentPageStart;
			Log.log(Log.DEBUG,this,"#3 - Current physical line is now " + currentPageStart);
		}

		printPage(_gfx,pageFormat,pageIndex,true);

		return PAGE_EXISTS;
	} 

	

	
	private static Color headerColor = Color.lightGray;
	private static Color headerTextColor = Color.black;
	private static Color footerColor = Color.lightGray;
	private static Color footerTextColor = Color.black;
	private static Color lineNumberColor = Color.gray;
	private static Color textColor = Color.black;
	

	
	private PrinterJob job;
	private Object format;

	private View view;
	private Buffer buffer;
	private Font font;
	private SyntaxStyle[] styles;
	private boolean header;
	private boolean footer;
	private boolean lineNumbers;

	private int currentPage;
	private int currentPageStart;
	private int currentPhysicalLine;
	private boolean end;

	private LineMetrics lm;
	private final List<Chunk> lineList;

	private FontRenderContext frc;

	private DisplayTokenHandler tokenHandler;
	

	
	private void printPage(Graphics _gfx, PageFormat pageFormat, int pageIndex,
		boolean actuallyPaint)
	{
		Log.log(Log.DEBUG,this,"printPage(" + pageIndex + ',' + actuallyPaint + ')');
		Graphics2D gfx = (Graphics2D)_gfx;
		gfx.setFont(font);

		double pageX = pageFormat.getImageableX();
		double pageY = pageFormat.getImageableY();
		double pageWidth = pageFormat.getImageableWidth();
		double pageHeight = pageFormat.getImageableHeight();

		Log.log(Log.DEBUG,this,"#1 - Page dimensions: " + pageWidth
			+ 'x' + pageHeight);

		if(header)
		{
			double headerHeight = paintHeader(gfx,pageX,pageY,pageWidth,
				actuallyPaint);
			pageY += headerHeight;
			pageHeight -= headerHeight;
		}

		if(footer)
		{
			double footerHeight = paintFooter(gfx,pageX,pageY,pageWidth,
				pageHeight,pageIndex,actuallyPaint);
			pageHeight -= footerHeight;
		}

		boolean glyphVector = jEdit.getBooleanProperty("print.glyphVector");
		double lineNumberWidth;

		
		if(lineNumbers)
		{
			
			
			int lineNumberDigits = (int)Math.ceil(Math.log(buffer.getLineCount() + 1)
				/ Math.log(10)) + 1;

			
			char[] chars = new char[lineNumberDigits];
			for(int i = 0; i < chars.length; i++)
				chars[i] = ' ';
			lineNumberWidth = font.getStringBounds(chars,
				0,lineNumberDigits,frc).getWidth();
		}
		else
			lineNumberWidth = 0.0;
		

		Log.log(Log.DEBUG,this,"#2 - Page dimensions: "
			+ (pageWidth - lineNumberWidth)
			+ 'x' + pageHeight);

		
		int tabSize = jEdit.getIntegerProperty("print.tabSize",8);
		char[] chars = new char[tabSize];
		for(int i = 0; i < chars.length; i++)
			chars[i] = ' ';
		double tabWidth = font.getStringBounds(chars,
			0,tabSize,frc).getWidth();
		PrintTabExpander e = new PrintTabExpander(tabWidth);
		

		lm = font.getLineMetrics("gGyYX",frc);
		Log.log(Log.DEBUG,this,"Line height is " + lm.getHeight());

		double y = 0.0;
print_loop:	for(;;)
		{
			if(currentPhysicalLine == buffer.getLineCount())
			{
				Log.log(Log.DEBUG,this,"Finished buffer");
				end = true;
				break print_loop;
			}
			if (!jEdit.getBooleanProperty("print.folds",true) &&
				!view.getTextArea().getDisplayManager().isLineVisible(currentPhysicalLine))
			{
				
				Log.log(Log.DEBUG,this,"Skipping invisible line");
				currentPhysicalLine++;
				continue;
			}
				
			lineList.clear();

			tokenHandler.init(styles,frc,e,lineList,
				(float)(pageWidth - lineNumberWidth));

			buffer.markTokens(currentPhysicalLine,tokenHandler);
			if(lineList.isEmpty())
				lineList.add(null);

			if(y + (lm.getHeight() * lineList.size()) >= pageHeight)
			{
				Log.log(Log.DEBUG,this,"Finished page before line " + currentPhysicalLine);
				break print_loop;
			}

			if(lineNumbers && actuallyPaint)
			{
				gfx.setFont(font);
				gfx.setColor(lineNumberColor);
				gfx.drawString(String.valueOf(currentPhysicalLine + 1),
					(float)pageX,(float)(pageY + y + lm.getHeight()));
			}

			for(int i = 0; i < lineList.size(); i++)
			{
				y += lm.getHeight();
				Chunk chunks = lineList.get(i);
				if(chunks != null && actuallyPaint)
				{
					Chunk.paintChunkBackgrounds(chunks,gfx,
						(float)(pageX + lineNumberWidth),
						(float)(pageY + y));
					Chunk.paintChunkList(chunks,gfx,
						(float)(pageX + lineNumberWidth),
						(float)(pageY + y),glyphVector);
				}
			}

			currentPhysicalLine++;
		}
	} 

	
	private double paintHeader(Graphics2D gfx, double pageX, double pageY,
		double pageWidth, boolean actuallyPaint)
	{
		String headerText = jEdit.getProperty("print.headerText",
			new String[] { buffer.getName() });
		FontRenderContext frc = gfx.getFontRenderContext();
		lm = font.getLineMetrics(headerText,frc);

		Rectangle2D bounds = font.getStringBounds(headerText,frc);
		Rectangle2D headerBounds = new Rectangle2D.Double(
			pageX,pageY,pageWidth,bounds.getHeight());

		if(actuallyPaint)
		{
			gfx.setColor(headerColor);
			gfx.fill(headerBounds);
			gfx.setColor(headerTextColor);
			gfx.drawString(headerText,
				(float)(pageX + (pageWidth - bounds.getWidth()) / 2),
				(float)(pageY + lm.getAscent()));
		}

		return headerBounds.getHeight();
	}
	

	
	private double paintFooter(Graphics2D gfx, double pageX, double pageY,
		double pageWidth, double pageHeight, int pageIndex,
		boolean actuallyPaint)
	{
		String footerText = jEdit.getProperty("print.footerText",
			new Object[] { new Date(), Integer.valueOf(pageIndex + 1)});
		FontRenderContext frc = gfx.getFontRenderContext();
		lm = font.getLineMetrics(footerText,frc);

		Rectangle2D bounds = font.getStringBounds(footerText,frc);
		Rectangle2D footerBounds = new Rectangle2D.Double(
			pageX,pageY + pageHeight - bounds.getHeight(),
			pageWidth,bounds.getHeight());

		if(actuallyPaint)
		{
			gfx.setColor(footerColor);
			gfx.fill(footerBounds);
			gfx.setColor(footerTextColor);
			gfx.drawString(footerText,
				(float)(pageX + (pageWidth - bounds.getWidth()) / 2),
				(float)(pageY + pageHeight - bounds.getHeight()
				+ lm.getAscent()));
		}

		return footerBounds.getHeight();
	} 

	

	
	static class PrintTabExpander implements TabExpander
	{
		private double tabWidth;

		
		PrintTabExpander(double tabWidth)
		{
			this.tabWidth = tabWidth;
		} 

		
		public float nextTabStop(float x, int tabOffset)
		{
			int ntabs = (int)((x + 1) / tabWidth);
			return (float)((ntabs + 1) * tabWidth);
		} 
	} 
}
