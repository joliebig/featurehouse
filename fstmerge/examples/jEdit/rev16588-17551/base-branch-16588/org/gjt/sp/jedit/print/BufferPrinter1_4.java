

package org.gjt.sp.jedit.print;


import javax.print.attribute.*;
import javax.print.attribute.standard.*;
import java.awt.print.*;
import java.awt.*;
import java.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;


public class BufferPrinter1_4
{
	
	private static PrinterJob getPrintJob(String jobName)
	{
		job = PrinterJob.getPrinterJob();

		format = new HashPrintRequestAttributeSet();

		String settings = jEdit.getSettingsDirectory();
		if(settings != null)
		{
			String printSpecPath = MiscUtilities.constructPath(
				settings, "printspec");
			File filePrintSpec = new File(printSpecPath);

			if (filePrintSpec.exists())
			{
				try
				{
					FileInputStream fileIn = new FileInputStream(filePrintSpec);
					ObjectInputStream obIn = new ObjectInputStream(fileIn);
					format = (HashPrintRequestAttributeSet)obIn.readObject();
				}
				catch(Exception e)
				{
					Log.log(Log.ERROR,BufferPrinter1_4.class,e);
				}
				
				if(jEdit.getBooleanProperty("print.color"))
					format.add(Chromaticity.COLOR);
				else
					format.add(Chromaticity.MONOCHROME);

				
				format.add(new JobName(jobName, null));
			}
		}

		return job;
	} 

	
	public static void pageSetup(View view)
	{
		PrinterJob prnJob = getPrintJob("PageSetupOnly");
		if(prnJob.pageDialog(format)!=null)
			savePrintSpec();
	} 

	
	public static void print(final View view, final Buffer buffer, boolean selection)
	{
		job = getPrintJob(buffer.getPath());

		boolean header = jEdit.getBooleanProperty("print.header");
		boolean footer = jEdit.getBooleanProperty("print.footer");
		boolean lineNumbers = jEdit.getBooleanProperty("print.lineNumbers");
		boolean color = jEdit.getBooleanProperty("print.color");
		Font font = jEdit.getFontProperty("print.font");

		BufferPrintable printable = new BufferPrintable(job,format,view,
			buffer,font,header,footer,lineNumbers,color);
		job.setPrintable(printable);

		if(!job.printDialog(format))
			return;

		savePrintSpec();

		printable.print();
	} 

	
	public static PageFormat getPageFormat()
	{
		
		PrinterJob prnJob=getPrintJob(" ");
		PageFormat pf=prnJob.defaultPage();
		Paper pap=pf.getPaper();

		MediaSizeName media=(MediaSizeName)format.get(
		                            Media.class);
		MediaSize ms=MediaSize.getMediaSizeForName(media);

		MediaPrintableArea mediaarea=(MediaPrintableArea)format.get(
		                                     MediaPrintableArea.class);
		if(mediaarea!=null)
			pap.setImageableArea((mediaarea.getX(MediaPrintableArea.INCH)*72),
			                     (mediaarea.getY(MediaPrintableArea.INCH)*72),
			                     (mediaarea.getWidth(MediaPrintableArea.INCH)*72),
			                     (mediaarea.getHeight(MediaPrintableArea.INCH)*72));
		if(ms!=null)
			pap.setSize((ms.getX(MediaSize.INCH)*72),
			            (ms.getY(MediaSize.INCH)*72));
		pf.setPaper(pap);

		OrientationRequested orientation=(OrientationRequested)format.get(
		                                         OrientationRequested.class);
		if(orientation!=null)
		{
			if(orientation.getValue()==OrientationRequested.LANDSCAPE.getValue())
			{
				pf.setOrientation(PageFormat.LANDSCAPE);
			}
			else if(orientation.getValue()==OrientationRequested.REVERSE_LANDSCAPE.getValue())
			{
				pf.setOrientation(PageFormat.REVERSE_LANDSCAPE);
			}
			else if(orientation.getValue()==OrientationRequested.PORTRAIT.getValue())
			{
				pf.setOrientation(PageFormat.PORTRAIT);
			}
			else if(orientation.getValue()==OrientationRequested.REVERSE_PORTRAIT.getValue())
			{
				
				
				
				pf.setOrientation(PageFormat.PORTRAIT);
			}
		}
		return pf;
	} 

	
	private static void savePrintSpec()
	{
		String settings = jEdit.getSettingsDirectory();
		if(settings == null)
			return;

		String printSpecPath = MiscUtilities.constructPath(
			settings, "printspec");
		File filePrintSpec = new File(printSpecPath);

		try
		{
			FileOutputStream fileOut=new FileOutputStream(filePrintSpec);
			ObjectOutputStream obOut=new ObjectOutputStream(fileOut);
			obOut.writeObject(format);
			
			Chromaticity cc=(Chromaticity)format.get(Chromaticity.class);
			if (cc!=null)
				jEdit.setBooleanProperty("print.color",
					cc.getValue()==Chromaticity.COLOR.getValue());
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}
	

	
	private static PrintRequestAttributeSet format;
	private static PrinterJob job;
	
}

