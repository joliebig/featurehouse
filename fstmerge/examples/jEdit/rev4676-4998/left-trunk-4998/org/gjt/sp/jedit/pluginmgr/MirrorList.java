

package org.gjt.sp.jedit.pluginmgr;

import com.microstar.xml.*;
import java.io.*;
import java.net.*;
import java.util.*;
import org.gjt.sp.jedit.*;

public class MirrorList
{
	public ArrayList mirrors;

	
	public MirrorList() throws Exception
	{
		mirrors = new ArrayList();

		Mirror none = new Mirror();
		none.id = Mirror.NONE;
		none.description = none.location = none.country = none.continent = "";
		mirrors.add(none);

		String path = jEdit.getProperty("plugin-manager.mirror-url");
		MirrorListHandler handler = new MirrorListHandler(this,path);
		XmlParser parser = new XmlParser();
		parser.setHandler(handler);

		Reader in = new BufferedReader(new InputStreamReader(
			new URL(path).openStream()));
		try
		{
			parser.parse(null,null,in);
		}
		finally
		{
			in.close();
		}
	} 

	

	
	void add(Mirror mirror)
	{
		mirrors.add(mirror);
	} 

	
	void finished()
	{
		Collections.sort(mirrors,new MirrorCompare());
	} 

	

	

	
	public static class Mirror
	{
		public static final String NONE = "NONE";

		public String id;
		public String description;
		public String location;
		public String country;
		public String continent;
	} 

	
	class MirrorCompare implements Comparator
	{
		public int compare(Object o1,Object o2)
		{
			Mirror m1 = (Mirror)o1;
			Mirror m2 = (Mirror)o2;

			int result;
			if ((result = m1.continent.compareToIgnoreCase(m2.continent)) == 0)
				if ((result = m1.country.compareToIgnoreCase(m2.country)) == 0)
					if ((result = m1.location.compareToIgnoreCase(m2.location)) == 0)
						return m1.description.compareToIgnoreCase(m2.description);
			return result;
		}

		public boolean equals(Object obj)
		{
			return (obj instanceof MirrorCompare);
		}
	} 

	
}
