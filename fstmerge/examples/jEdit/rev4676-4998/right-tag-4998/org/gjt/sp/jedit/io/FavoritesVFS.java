

package org.gjt.sp.jedit.io;


import java.awt.Component;
import java.util.ArrayList;
import org.gjt.sp.jedit.jEdit;



public class FavoritesVFS extends VFS
{
	public static final String PROTOCOL = "favorites";

	
	public FavoritesVFS()
	{
		
		
		super("favorites",DELETE_CAP | LOW_LATENCY_CAP);

		
		instance = this;
	} 

	
	public String getParentOfPath(String path)
	{
		return PROTOCOL + ":";
	} 

	
	public VFS.DirectoryEntry[] _listDirectory(Object session, String url,
		Component comp)
	{
		synchronized(lock)
		{
			if(favorites == null)
				loadFavorites();

			VFS.DirectoryEntry[] retVal = new VFS.DirectoryEntry[favorites.size()];
			for(int i = 0; i < retVal.length; i++)
			{
				String favorite = (String)favorites.get(i);
				retVal[i] = _getDirectoryEntry(session,favorite,comp);
			}
			return retVal;
		}
	} 

	
	public DirectoryEntry _getDirectoryEntry(Object session, String path,
		Component comp)
	{
		return new VFS.DirectoryEntry(path,path,"favorites:" + path,
					VFS.DirectoryEntry.DIRECTORY,
					0L,false);
	} 

	
	public boolean _delete(Object session, String path, Component comp)
	{
		synchronized(lock)
		{
			path = path.substring(PROTOCOL.length() + 1);
			favorites.remove(path);

			VFSManager.sendVFSUpdate(this,PROTOCOL + ":",false);
		}

		return true;
	} 

	
	public static void loadFavorites()
	{
		favorites = new ArrayList();

		synchronized(lock)
		{
			String favorite;
			int i = 0;
			while((favorite = jEdit.getProperty("vfs.favorite." + i)) != null)
			{
				favorites.add(favorite);
				i++;
			}
		}
	} 

	
	public static void addToFavorites(String path)
	{
		synchronized(lock)
		{
			if(favorites == null)
				loadFavorites();

			if(!favorites.contains(path))
				favorites.add(path);

			VFSManager.sendVFSUpdate(instance,PROTOCOL + ":",false);
		}
	} 

	
	public static void saveFavorites()
	{
		synchronized(lock)
		{
			if(favorites == null)
				return;

			for(int i = 0; i < favorites.size(); i++)
			{
				jEdit.setProperty("vfs.favorite." + i,
					(String)favorites.get(i));
			}
			jEdit.unsetProperty("vfs.favorite." + favorites.size());
		}
	} 

	
	public static Object[] getFavorites()
	{
		synchronized(lock)
		{
			if(favorites == null)
				loadFavorites();

			return favorites.toArray();
		}
	} 

	
	private static FavoritesVFS instance;
	private static Object lock = new Object();
	private static ArrayList favorites;
	
}
