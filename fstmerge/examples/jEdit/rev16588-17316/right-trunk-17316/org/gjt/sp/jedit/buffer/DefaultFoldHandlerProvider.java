
package org.gjt.sp.jedit.buffer;

import java.util.Map;
import java.util.HashMap;


public class DefaultFoldHandlerProvider implements FoldHandlerProvider
{
	private final Map<String, FoldHandler> folds = new HashMap<String, FoldHandler>();
	
	public FoldHandler getFoldHandler(String name)
	{
		return folds.get(name);
	}

	
	public String[] getFoldModes()
	{
		return folds.keySet().toArray(new String[folds.size()]); 
	}
	
	
	public void addFoldHandler(FoldHandler foldHandler)
	{
		folds.put(foldHandler.getName(), foldHandler);
	}
}
