

package org.gjt.sp.jedit.gui;


import javax.swing.DefaultListModel;
import java.util.*;



public class HistoryModel extends DefaultListModel
	implements MutableListModel
{
	
	
	public HistoryModel(String name)
	{
		this.name = name;
	} 

	
	
	public void addItem(String text)
	{
		if(text == null || text.length() == 0)
			return;

		int index = indexOf(text);
		if(index != -1)
			removeElementAt(index);

		insertElementAt(text,0);

		while(getSize() > max)
			removeElementAt(getSize() - 1);
	} 

	
	public void insertElementAt(Object obj, int index)
	{
		modified = true;
		super.insertElementAt(obj,index);
	} 

	
	
	public String getItem(int index)
	{
		return (String)elementAt(index);
	} 

	
	public boolean removeElement(Object obj)
	{
		modified = true;
		return super.removeElement(obj);
	} 

	
	
	public void clear()
	{
		removeAllElements();
	} 

	
	public void removeAllElements()
	{
		modified = true;
		super.removeAllElements();
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public static HistoryModel getModel(String name)
	{
		if(models == null)
			models = Collections.synchronizedMap(new HashMap<String, HistoryModel>());

		HistoryModel model = models.get(name);
		if(model == null)
		{
			model = new HistoryModel(name);
			models.put(name,model);
		}

		return model;
	} 

	
	public static void loadHistory()
	{
		if (saver != null)
			models = saver.load(models);
	} 

	
	public static void saveHistory()
	{
		if (saver != null && modified && saver.save(models))
			modified = false;
	} 

	
	public static void setMax(int max)
	{
		HistoryModel.max = max;
	} 

	
	public static void setSaver(HistoryModelSaver saver)
	{
		HistoryModel.saver = saver;
	} 

	
	private static int max;

	private String name;
	private static Map<String, HistoryModel> models;

	private static boolean modified;
	private static HistoryModelSaver saver;
	
}
