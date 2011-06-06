

package org.gjt.sp.jedit.io;


import java.awt.Component;
import java.util.*;
import org.gjt.sp.jedit.msg.DynamicMenuChanged;
import org.gjt.sp.jedit.*;



public class FavoritesVFS extends VFS
{
	public static final String PROTOCOL = "favorites";

	
	public FavoritesVFS()
	{
		super("favorites",DELETE_CAP | LOW_LATENCY_CAP,
			new String[] { EA_TYPE });

		
		instance = this;
	} 

	
	public String getParentOfPath(String path)
	{
		return PROTOCOL + ":";
	} 

	
	public VFS.DirectoryEntry[] _listDirectory(Object session, String url,
		Component comp)
	{
		return getFavorites();
	} 

	
	public DirectoryEntry _getDirectoryEntry(Object session, String path,
		Component comp)
	{
		
		return new FavoritesEntry(path,VFS.DirectoryEntry.DIRECTORY);
	} 

	
	public boolean _delete(Object session, String path, Component comp)
	{
		synchronized(lock)
		{
			path = path.substring(PROTOCOL.length() + 1);

			Iterator iter = favorites.iterator();
			while(iter.hasNext())
			{
				if(((FavoritesEntry)iter.next()).path.equals(path))
				{
					iter.remove();
					VFSManager.sendVFSUpdate(this,PROTOCOL
						+ ":",false);
					EditBus.send(new DynamicMenuChanged(
						"favorites"));
					return true;
				}
			}
		}

		return false;
	} 

	
	public static void loadFavorites()
	{
		synchronized(lock)
		{
			favorites = new LinkedList();

			String favorite;
			int i = 0;
			while((favorite = jEdit.getProperty("vfs.favorite." + i)) != null)
			{
				favorites.add(new FavoritesEntry(favorite,
					jEdit.getIntegerProperty("vfs.favorite."
					+ i + ".type",
					VFS.DirectoryEntry.DIRECTORY)));
				i++;
			}
		}
	} 

	
	public static void addToFavorites(String path, int type)
	{
		synchronized(lock)
		{
			if(favorites == null)
				loadFavorites();

			Iterator iter = favorites.iterator();
			while(iter.hasNext())
			{
				if(((FavoritesEntry)iter.next()).path.equals(path))
					return;
			}

			favorites.add(new FavoritesEntry(path,type));

			VFSManager.sendVFSUpdate(instance,PROTOCOL + ":",false);
			EditBus.send(new DynamicMenuChanged("favorites"));
		}
	} 

	
	public static void saveFavorites()
	{
		synchronized(lock)
		{
			if(favorites == null)
				return;

			int i = 0;
			Iterator iter = favorites.iterator();
			while(iter.hasNext())
			{
				FavoritesEntry e = ((FavoritesEntry)
					iter.next());
				jEdit.setProperty("vfs.favorite." + i,
					e.path);
				jEdit.setIntegerProperty("vfs.favorite." + i
					+ ".type",e.type);

				i++;
			}
			jEdit.unsetProperty("vfs.favorite." + favorites.size());
			jEdit.unsetProperty("vfs.favorite." + favorites.size()
				+ ".type");
		}
	} 

	
	public static VFS.DirectoryEntry[] getFavorites()
	{
		synchronized(lock)
		{
			if(favorites == null)
				loadFavorites();

			return (VFS.DirectoryEntry[])favorites.toArray(
				new VFS.DirectoryEntry[favorites.size()]);
		}
	} 

	
	private static FavoritesVFS instance;
	private static Object lock = new Object();
	private static List favorites;
	

	
	static class FavoritesEntry extends VFS.DirectoryEntry
	{
		FavoritesEntry(String path, int type)
		{
			super(path,path,PROTOCOL + ":" + path,type,0,false);
		}

		public String getExtendedAttribute(String name)
		{
			if(name.equals(EA_TYPE))
				return super.getExtendedAttribute(name);
			else
			{
				
				
				return null;
			}
		}
	} 
}
