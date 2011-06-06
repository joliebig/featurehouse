

package org.gjt.sp.jedit;


import java.io.File;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;

import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.util.XMLUtilities;



public class SettingsXML
{
	
	
	public class Saver extends BufferedWriter
	{
		
		
		public void writeXMLDeclaration() throws IOException
		{
			writeXMLDeclaration("1.0");
		} 

		
		
		public void writeXMLDeclaration(String version)
			throws IOException
		{
			write("<?xml"
				+ " version=\"" + version + "\""
				+ " encoding=\"" + encoding + "\""
				+ " ?>");
			newLine();
		} 

		
		
		public void finish() throws IOException
		{
			close();
			jEdit.backupSettingsFile(file);
			file.delete();
			twoStageSaveFile.renameTo(file);
			knownLastModified = file.lastModified();
		} 

		
		private File twoStageSaveFile;
		private static final String encoding = "UTF-8";

		
		Saver() throws IOException
		{
			this(new File(file.getParentFile(),
				"#" + file.getName() + "#save#"));
		}

		
		private Saver(File twoStageSaveFile) throws IOException
		{
			super(new OutputStreamWriter(
				new FileOutputStream(twoStageSaveFile)
				, encoding));
			this.twoStageSaveFile = twoStageSaveFile;
		}

		
	} 

	
	
	public SettingsXML(String settingsDirectory, String name)
	{
		String filename = name + ".xml";
		file = new File(MiscUtilities.constructPath(
			settingsDirectory, filename));
	} 
	
	public SettingsXML(File f)
	{
		file = f;
	}

	
	
	public boolean fileExists()
	{
		return file.exists();
	} 

	
	
	public void load(DefaultHandler handler) throws IOException
	{
		XMLUtilities.parseXML(new FileInputStream(file), handler);
		knownLastModified = file.lastModified();
	} 

	
	
	public Saver openSaver() throws IOException
	{
		return new Saver();
	} 

	
	
	public boolean hasChangedOnDisk()
	{
		return file.exists()
			&& (file.lastModified() != knownLastModified);
	} 

	
	
	public String toString()
	{
		return file.toString();
	} 

	
	private File file;
	private long knownLastModified;
	
}
