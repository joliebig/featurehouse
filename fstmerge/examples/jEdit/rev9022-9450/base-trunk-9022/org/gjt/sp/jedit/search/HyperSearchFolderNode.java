
package org.gjt.sp.jedit.search;

import java.io.File;

public class HyperSearchFolderNode 
{
	private File nodeFile;
	private boolean showFullPath;
	private static String fileSep = System.getProperty("file.separator");
	static
	{
		if (fileSep.equals("\\"))
			fileSep = "\\\\";
	}
	
	public File getNodeFile() {
		return nodeFile;
	}
	
	public HyperSearchFolderNode(File nodeFile, boolean showFullPath) {
		this.nodeFile = nodeFile;
		this.showFullPath = showFullPath;
	}
	
	public String toString() {
		if (showFullPath)
			return nodeFile.getAbsolutePath();
		String paths[] = nodeFile.getAbsolutePath().split(fileSep);
		return paths[paths.length - 1];
		
	}
}
