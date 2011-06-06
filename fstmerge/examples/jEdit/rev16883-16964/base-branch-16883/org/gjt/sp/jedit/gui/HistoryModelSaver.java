
package org.gjt.sp.jedit.gui;

import java.util.Map;


public interface HistoryModelSaver
{
	Map<String, HistoryModel> load(Map<String, HistoryModel> models);

	boolean save(Map<String, HistoryModel> models);
}
