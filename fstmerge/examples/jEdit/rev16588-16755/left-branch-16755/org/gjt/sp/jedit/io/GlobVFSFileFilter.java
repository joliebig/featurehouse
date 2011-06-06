

package org.gjt.sp.jedit.io;

import java.util.regex.Pattern;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.util.StandardUtilities;


public class GlobVFSFileFilter implements VFSFileFilter
{

	public GlobVFSFileFilter(String glob)
	{
		this.glob = glob;
	}

	public boolean accept(VFSFile file)
	{
		if (file.getType() == VFSFile.DIRECTORY
				|| file.getType() == VFSFile.FILESYSTEM)
		{
			return true;
		}
		else
		{
			return accept(file.getName());
		}
	}

	public boolean accept(String url)
	{
		if (pattern == null)
		{
			pattern = Pattern.compile(StandardUtilities.globToRE(glob),
						  Pattern.CASE_INSENSITIVE);
		}
		return pattern.matcher(url).matches();
	}

	public String getDescription()
	{
		return jEdit.getProperty("vfs.browser.file_filter.glob");
	}

	public String toString()
	{
		return glob;
	}

	public void setGlob(String glob)
	{
		this.glob = glob;
		pattern = null;
	}

	public String getGlob()
	{
		return glob;
	}

	private String glob;
	private Pattern pattern;

}

