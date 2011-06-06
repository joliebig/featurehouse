

package org.gjt.sp.util;


public interface ProgressObserver
{
	
	void setValue(long value);
	 
	
	void setMaximum(long value);
	 
	
	void setStatus(String status);
}
