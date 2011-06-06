

package org.gjt.sp.jedit.io;


public interface VFSFileFilter {

	public static final String SERVICE_NAME = VFSFileFilter.class.getName();

	
	public boolean accept(VFSFile file);

	
	public boolean accept(String url);

	
	public String getDescription();

}

