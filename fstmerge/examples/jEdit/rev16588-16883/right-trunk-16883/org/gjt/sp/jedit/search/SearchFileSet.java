

package org.gjt.sp.jedit.search;

import org.gjt.sp.jedit.*;


public interface SearchFileSet
{
	
	String getFirstFile(View view);

	
	String getNextFile(View view, String path);

	
	String[] getFiles(View view);

	
	int getFileCount(View view);

	
	String getCode();
}
