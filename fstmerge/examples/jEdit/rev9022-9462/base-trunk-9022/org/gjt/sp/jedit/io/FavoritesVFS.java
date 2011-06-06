

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

	
	public VFSFile[] _listFiles(Object session, String url,
		Component comp)
	{
		return getFavorites();
	} 

	
	public VFSFile _getFile(Object session, String path,
		Component comp)
	{
		
		return new Favorite(path,VFSFile.DIRECTORY);
	} 

	
	public boolean _delete(Object session, String path, Component comp)
	{
		synchronized(lock)
		{
			path = path.substring(PROTOCOL.length() + 1);

			Iterator iter = favorites.iterator();
			while(iter.hasNext())
			{
				if(((Favorite)iter.next()).getPath()
					.equals(path))
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
				favorites.add(new Favorite(favorite,
					jEdit.getIntegerProperty("vfs.favorite."
					+ i + ".type",
					VFSFile.DIRECTORY)));
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
				if(((Favorite)iter.next()).getPath().equals(path))
					return;
			}

			favorites.add(new Favorite(path,type));

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
				Favorite e = ((Favorite)iter.next());
				jEdit.setProperty("vfs.favorite." + i,
					e.getPath());
				jEdit.setIntegerProperty("vfs.favorite." + i
					+ ".type",e.getType());

				i++;
			}
			jEdit.unsetProperty("vfs.favorite." + favorites.size());
			jEdit.unsetProperty("vfs.favorite." + favorites.size()
				+ ".type");
		}
	} 

	
	public static VFSFile[] getFavorites()
	{
		synchronized(lock)
		{
			if(favorites == null)
				loadFavorites();

			return (VFSFile[])favorites.toArray(
				new VFSFile[favorites.size()]);
		}
	} 

	
	private static FavoritesVFS instance;
	private static Object lock = new Object();
	private static List favorites;
	

	
	static class Favorite extends VFSFile
	{
		Favorite(String path, int type)
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
