
package org.gjt.sp.jedit;

import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;
import org.gjt.sp.util.IOUtilities;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Attributes;

import java.io.*;


public class JEditRegisterSaver implements RegisterSaver
{
	
	public void loadRegisters()
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			return;

		File registerFile = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(),"registers.xml"));
		if(!registerFile.exists())
			return;

		registersModTime = registerFile.lastModified();

		Log.log(Log.MESSAGE,jEdit.class,"Loading registers.xml");

		RegistersHandler handler = new RegistersHandler();
		try
		{
			Registers.setLoading(true);
			XMLUtilities.parseXML(new FileInputStream(registerFile),
						handler);
		}
		catch (IOException ioe)
		{
			Log.log(Log.ERROR, Registers.class, ioe);
		}
		finally
		{
			Registers.setLoading(false);
		}
	} 

	
	public void saveRegisters()
	{

		Log.log(Log.MESSAGE,Registers.class,"Saving registers.xml");
		File file1 = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(), "#registers.xml#save#"));
		File file2 = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(), "registers.xml"));
		if(file2.exists() && file2.lastModified() != registersModTime)
		{
			Log.log(Log.WARNING,Registers.class,file2 + " changed"
				+ " on disk; will not save registers");
			return;
		}

		jEdit.backupSettingsFile(file2);

		String lineSep = System.getProperty("line.separator");

		BufferedWriter out = null;

		boolean ok = false;

		try
		{
			out = new BufferedWriter(new FileWriter(file1));

			out.write("<?xml version=\"1.0\"?>");
			out.write(lineSep);
			out.write("<!DOCTYPE REGISTERS SYSTEM \"registers.dtd\">");
			out.write(lineSep);
			out.write("<REGISTERS>");
			out.write(lineSep);

			Registers.Register[] registers = Registers.getRegisters();
			for(int i = 0; i < registers.length; i++)
			{
				Registers.Register register = registers[i];
				if(register == null ||
                                   i == '$' ||
                                   i == '%' ||
                                   register.toString().length() == 0
                                  )
					continue;

				out.write("<REGISTER NAME=\"");
				if(i == '"')
					out.write("&quot;");
				else
					out.write((char)i);
				out.write("\">");

				out.write(XMLUtilities.charsToEntities(
					register.toString(), false));

				out.write("</REGISTER>");
				out.write(lineSep);
			}

			out.write("</REGISTERS>");
			out.write(lineSep);

			ok = true;
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,Registers.class,e);
		}
		finally
		{
			IOUtilities.closeQuietly(out);
		}

		if(ok)
		{
			
			file2.delete();
			file1.renameTo(file2);
		}

		registersModTime = file2.lastModified();
	} 

	private static long registersModTime;

	
	static class RegistersHandler extends DefaultHandler
		{
		
		public InputSource resolveEntity(String publicId, String systemId)
		{
			return XMLUtilities.findEntity(systemId, "registers.dtd", getClass());
		} 

		
		public void startElement(String uri, String localName,
					 String qName, Attributes attrs)
		{
			registerName = attrs.getValue("NAME");
			inRegister = "REGISTER".equals(qName);
		} 

		
		public void endElement(String uri, String localName, String name)
		{
			if(name.equals("REGISTER"))
			{
				if(registerName == null || registerName.length() != 1)
					Log.log(Log.ERROR,this,"Malformed NAME: " + registerName);
				else
					Registers.setRegister(registerName.charAt(0),charData.toString());
				inRegister = false;
				charData.setLength(0);
			}
		} 

		
		public void characters(char[] ch, int start, int length)
		{
			if (inRegister)
				charData.append(ch, start, length);
		} 

		
		private String registerName;
		private StringBuffer charData = new StringBuffer();
		private boolean inRegister;
		
	} 
}
