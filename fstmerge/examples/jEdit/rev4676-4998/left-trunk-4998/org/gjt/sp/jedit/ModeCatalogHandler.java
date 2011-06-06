

package org.gjt.sp.jedit;

import com.microstar.xml.*;
import java.io.*;
import org.gjt.sp.util.Log;

class ModeCatalogHandler extends HandlerBase
{
	ModeCatalogHandler(String directory, boolean resource)
	{
		this.directory = directory;
		this.resource = resource;
	}

	public Object resolveEntity(String publicId, String systemId)
	{
		if("catalog.dtd".equals(systemId))
		{
			
			
			
			return new StringReader("<!-- -->");

			
		}

		return null;
	}

	public void attribute(String aname, String value, boolean isSpecified)
	{
		aname = (aname == null) ? null : aname.intern();

		if(aname == "NAME")
			modeName = value;
		else if(aname == "FILE")
		{
			if(value == null)
			{
				Log.log(Log.ERROR,this,directory + "catalog:"
					+ " mode " + modeName + " doesn't have"
					+ " a FILE attribute");
			}
			else
				file = value;
		}
		else if(aname == "FILE_NAME_GLOB")
			filenameGlob = value;
		else if(aname == "FIRST_LINE_GLOB")
			firstlineGlob = value;
	}

	public void doctypeDecl(String name, String publicId,
		String systemId) throws Exception
	{
		
		
		

		
		
		
		if("CATALOG".equals(name) || "MODES".equals(name))
			return;

		Log.log(Log.ERROR,this,directory + "catalog: DOCTYPE must be CATALOG");
	}

	public void endElement(String name)
	{
		if(name.equals("MODE"))
		{
			Mode mode = jEdit.getMode(modeName);
			if(mode == null)
			{
				mode = new Mode(modeName);
				jEdit.addMode(mode);
			}

			Object path;
			if(resource)
				path = jEdit.class.getResource(directory + file);
			else
				path = MiscUtilities.constructPath(directory,file);
			mode.setProperty("file",path);

			if(filenameGlob != null)
				mode.setProperty("filenameGlob",filenameGlob);
			else
				mode.unsetProperty("filenameGlob");

			if(firstlineGlob != null)
				mode.setProperty("firstlineGlob",firstlineGlob);
			else
				mode.unsetProperty("firstlineGlob");

			mode.init();

			modeName = file = filenameGlob = firstlineGlob = null;
		}
	}

	

	
	private String directory;
	private boolean resource;

	private String modeName;
	private String file;
	private String filenameGlob;
	private String firstlineGlob;
}
