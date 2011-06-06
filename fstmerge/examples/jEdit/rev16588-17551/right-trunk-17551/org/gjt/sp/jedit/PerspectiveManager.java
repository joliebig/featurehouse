

package org.gjt.sp.jedit;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;

import org.gjt.sp.util.IOUtilities;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;


public class PerspectiveManager
{
	private static final String PERSPECTIVE_FILENAME = "perspective";

	
	
	public static boolean isPerspectiveDirty()
	{
		return dirty;
	} 

	
	
	public static void setPerspectiveDirty(boolean dirty)
	{
		PerspectiveManager.dirty = dirty;
	} 

	
	
	public static boolean isPerspectiveEnabled()
	{
		return enabled;
	} 

	
	
	public static void setPerspectiveEnabled(boolean enabled)
	{
		PerspectiveManager.enabled = enabled;
	} 

	
	public static View loadPerspective(boolean restoreFiles)
	{
		if(perspectiveXML == null)
			return null;

		if(!perspectiveXML.fileExists())
			return null;

		Log.log(Log.MESSAGE,PerspectiveManager.class,"Loading " + perspectiveXML);

		PerspectiveHandler handler = new PerspectiveHandler(restoreFiles);
		try
		{
			perspectiveXML.load(handler);
		}
		catch(IOException e)
		{
			Log.log(Log.ERROR,PerspectiveManager.class,e);
		}
		return handler.view;
	} 

	
	public static void savePerspective(boolean autosave)
	{
		if(!isPerspectiveEnabled() || !jEdit.isStartupDone())
			return;

		if(perspectiveXML == null)
			return;
		
		
		if(jEdit.getBufferCount() == 0)
			return;

		Buffer[] buffers = jEdit.getBuffers();
		Collection<Buffer> savedBuffers = new LinkedList<Buffer>();
		for (Buffer buffer: buffers)
		{
			if (!buffer.isNewFile())
			{
				savedBuffers.add(buffer);
			}
		}

		if(!autosave)
			Log.log(Log.MESSAGE,PerspectiveManager.class,"Saving " + perspectiveXML);

		String lineSep = System.getProperty("line.separator");

		SettingsXML.Saver out = null;

		try
		{
			out = perspectiveXML.openSaver();
			out.writeXMLDeclaration();

			out.write("<!DOCTYPE PERSPECTIVE SYSTEM \"perspective.dtd\">");
			out.write(lineSep);
			out.write("<PERSPECTIVE>");
			out.write(lineSep);

			for (Buffer buffer: savedBuffers)
			{
				out.write("<BUFFER AUTORELOAD=\"");
				out.write(buffer.getAutoReload() ? "TRUE" : "FALSE");
				out.write("\" AUTORELOAD_DIALOG=\"");
				out.write(buffer.getAutoReloadDialog() ? "TRUE" : "FALSE");
				out.write("\">");
				out.write(XMLUtilities.charsToEntities(buffer.getPath(), false));
				out.write("</BUFFER>");
				out.write(lineSep);
			}

			View[] views = jEdit.getViews();
			for(int i = 0; i < views.length; i++)
			{
				View view = views[i];
				
				
				
				if(view == jEdit.getActiveView()
					&& i != views.length - 1)
				{
					View last = views[views.length - 1];
					views[i] = last;
					views[views.length - 1] = view;
					view = last;
				}

				View.ViewConfig config = views[i].getViewConfig();
				out.write("<VIEW PLAIN=\"");
				out.write(config.plainView ? "TRUE" : "FALSE");
				out.write("\">");

				if (config.title != null)
				{
					out.write(lineSep);
					out.write("<TITLE>");
					out.write(XMLUtilities.charsToEntities(config.title,false));
					out.write("</TITLE>");
					out.write(lineSep);
				}

				out.write("<PANES>");
				out.write(lineSep);
				out.write(XMLUtilities.charsToEntities(
					config.splitConfig,false));
				out.write(lineSep);
				out.write("</PANES>");
				out.write(lineSep);

				out.write("<GEOMETRY X=\"");
				out.write(String.valueOf(config.x));
				out.write("\" Y=\"");
				out.write(String.valueOf(config.y));
				out.write("\" WIDTH=\"");
				out.write(String.valueOf(config.width));
				out.write("\" HEIGHT=\"");
				out.write(String.valueOf(config.height));
				out.write("\" EXT_STATE=\"");
				out.write(String.valueOf(config.extState));
				out.write("\" />");
				out.write(lineSep);

				if (config.docking != null)
					config.docking.saveLayout(PERSPECTIVE_FILENAME, i);
				
				out.write("</VIEW>");
				out.write(lineSep);
			}

			out.write("</PERSPECTIVE>");
			out.write(lineSep);

			out.finish();
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,PerspectiveManager.class,"Error saving " + perspectiveXML);
			Log.log(Log.ERROR,PerspectiveManager.class,io);
		}
		finally
		{
			IOUtilities.closeQuietly(out);
		}
	} 

	
	private static boolean dirty, enabled = true;
	private static SettingsXML perspectiveXML;

	
	static
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory != null)
		{
			perspectiveXML = new SettingsXML(settingsDirectory, PERSPECTIVE_FILENAME);
		}
	} 

	
	private static class PerspectiveHandler extends DefaultHandler
	{
		View view;
		private StringBuilder charData;
		View.ViewConfig config;
		boolean restoreFiles;
		String autoReload, autoReloadDialog;
		
		PerspectiveHandler(boolean restoreFiles)
		{
			this.restoreFiles = restoreFiles;
			config = new View.ViewConfig();
			charData = new StringBuilder();
			config.docking = View.getDockingFrameworkProvider().createDockingLayout();
		}

		@Override
		public InputSource resolveEntity(String publicId, String systemId)
		{
			return XMLUtilities.findEntity(systemId, "perspective.dtd", getClass());
		}

		@Override
		public void startElement(String uri, String localName,
					 String qName, Attributes attrs)
		{
			charData.setLength(0);
			for (int i = 0; i < attrs.getLength(); i++)
			{
				String name = attrs.getQName(i);
				String value = attrs.getValue(i);
				attribute(name, value);
			}
		}

		private void attribute(String aname, String value)
		{
			if(aname.equals("X"))
				config.x = Integer.parseInt(value);
			else if(aname.equals("Y"))
				config.y = Integer.parseInt(value);
			else if(aname.equals("WIDTH"))
				config.width = Integer.parseInt(value);
			else if(aname.equals("HEIGHT"))
				config.height = Integer.parseInt(value);
			else if(aname.equals("EXT_STATE"))
				config.extState = Integer.parseInt(value);
			else if(aname.equals("PLAIN"))
				config.plainView = ("TRUE".equals(value));
			else if(aname.equals("AUTORELOAD"))
				autoReload = value;
			else if(aname.equals("AUTORELOAD_DIALOG"))
				autoReloadDialog = value;
		}

		
		public static boolean skipRemote(String uri)
		{
			if (jEdit.getBooleanProperty("restore.remote"))
				return false;
			if(MiscUtilities.isURL(uri))
			{
				String protocol = MiscUtilities.getProtocolOfURL(uri);
				if (!protocol.equals("file")) return true;
			}
			return false;
		}

		@Override
		public void endElement(String uri, String localName, String name)
		{
			if(name.equals("BUFFER"))
			{
				if (restoreFiles && !skipRemote(charData.toString()))
				{
					Buffer restored = jEdit.openTemporary(null,null, charData.toString(), false);
					
					
					if (restored != null)
					{
						if(autoReload != null)
							restored.setAutoReload("TRUE".equals(autoReload));
						if(autoReloadDialog != null)
							restored.setAutoReloadDialog("TRUE".equals(autoReloadDialog));
						jEdit.commitTemporary(restored);
					}
				}
			}
			else if(name.equals("PANES"))
				config.splitConfig = charData.toString();
			else if(name.equals("VIEW"))
			{
				if (config.docking != null)
					config.docking.loadLayout(PERSPECTIVE_FILENAME, jEdit.getViewCount());
				view = jEdit.newView(view,null,config);
				config = new View.ViewConfig();
				config.docking = View.getDockingFrameworkProvider().createDockingLayout();
			}
			else if(name.equals("TITLE"))
				config.title = charData.toString();
		}

		@Override
		public void characters(char[] ch, int start, int length)
		{
			charData.append(ch,start,length);
		}
	} 

	
}
