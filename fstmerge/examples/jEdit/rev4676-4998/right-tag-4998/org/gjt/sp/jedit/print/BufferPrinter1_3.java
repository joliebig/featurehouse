

package org.gjt.sp.jedit.print;


import java.awt.print.*;
import java.awt.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.io.VFSManager;


public class BufferPrinter1_3
{
	
	private static PrinterJob getPrintJob()
	{
		job = PrinterJob.getPrinterJob();

		int orientation = jEdit.getIntegerProperty("print.orientation",PageFormat.PORTRAIT);
		double width = jEdit.getDoubleProperty("print.width",0);
		double height = jEdit.getDoubleProperty("print.height",0);
		double x = jEdit.getDoubleProperty("print.x",0);
		double y = jEdit.getDoubleProperty("print.y",0);
		double pagewidth = jEdit.getDoubleProperty("print.pagewidth",0);
		double pageheight = jEdit.getDoubleProperty("print.pageheight",0);

		format = job.defaultPage();
		
		if(width!=0 && height!=0 )
		{
			Paper pap = format.getPaper();
			pap.setImageableArea(x,y,width,height);
			pap.setSize(pagewidth,pageheight);
			format.setPaper(pap);
		}
		format.setOrientation(orientation);
		return job;

	}

	
	public static void pageSetup(View view)
	{
		job = getPrintJob();

		PageFormat newFormat = job.pageDialog(format);
		if(newFormat != null)
		{
			format = newFormat;
			jEdit.setIntegerProperty("print.orientation",format.getOrientation());
			Paper paper=format.getPaper();

			jEdit.setDoubleProperty("print.width",paper.getImageableWidth());
			jEdit.setDoubleProperty("print.height",paper.getImageableHeight());
			jEdit.setDoubleProperty("print.x",paper.getImageableX());
			jEdit.setDoubleProperty("print.y",paper.getImageableY());
			jEdit.setDoubleProperty("print.pagewidth",paper.getWidth());
			jEdit.setDoubleProperty("print.pageheight",paper.getHeight());
		}
	} 

	
	public static void print(final View view, final Buffer buffer, boolean selection)
	{
		job = getPrintJob();
		job.setJobName(buffer.getPath());
		boolean header = jEdit.getBooleanProperty("print.header");
		boolean footer = jEdit.getBooleanProperty("print.footer");
		boolean lineNumbers = jEdit.getBooleanProperty("print.lineNumbers");
		boolean color = jEdit.getBooleanProperty("print.color");
		Font font = jEdit.getFontProperty("print.font");

		BufferPrintable printable = new BufferPrintable(job,null,view,
			buffer,font,header,footer,lineNumbers,color);
		job.setPrintable(printable,format);

		if(!job.printDialog())
			return;

		VFSManager.runInWorkThread(printable);
	} 

	
	public static PageFormat getPageFormat()
	{
		return format;
	} 

	
	private static PageFormat format;
	private static PrinterJob job;
	
}
