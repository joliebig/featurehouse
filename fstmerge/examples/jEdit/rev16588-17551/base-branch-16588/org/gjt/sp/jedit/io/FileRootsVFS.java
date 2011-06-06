

package org.gjt.sp.jedit.io;


import javax.swing.filechooser.FileSystemView;
import java.awt.Component;
import java.io.File;
import java.util.LinkedList;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.OperatingSystem;



public class FileRootsVFS extends VFS
{
	public static final String PROTOCOL = "roots";

	
	public FileRootsVFS()
	{
		super("roots",LOW_LATENCY_CAP | BROWSE_CAP, new String[] {
			EA_TYPE });
	} 

	
	public String getParentOfPath(String path)
	{
		return PROTOCOL + ':';
	} 

	
	public VFSFile[] _listFiles(Object session, String url,
		Component comp)
	{
		File[] roots = listRoots();

		if(roots == null)
			return null;

		VFSFile[] rootDE = new VFSFile[roots.length];
		for(int i = 0; i < roots.length; i++)
			rootDE[i] = new Root(roots[i]);

		return rootDE;
	} 

	
	public VFSFile _getFile(Object session, String path,
		Component comp)
	{
		return new Root(new File(path));
	} 

	
	private static FileSystemView fsView = FileSystemView.getFileSystemView();

	
	private static File[] listRoots()
	{
		if (OperatingSystem.isMacOS())
		{
			
			File[] volumes = new File("/Volumes").listFiles();
			LinkedList<File> roots = new LinkedList<File>();

			roots.add(new File("/"));

			for (int i=0; i<volumes.length; i++)
			{
				
				if (volumes[i].isDirectory())
					roots.add(volumes[i]);
			}

			return roots.toArray(new File[roots.size()]);
		}
		else
		{
			File[] roots = File.listRoots();
			File[] desktop = fsView.getRoots();

			if(desktop == null)
				return roots;

			File[] rootsPlus = new File[roots.length + desktop.length];
			System.arraycopy(desktop, 0, rootsPlus, 0, desktop.length);
			System.arraycopy(roots, 0, rootsPlus, 1, roots.length);
			return rootsPlus;
		}
	} 

	

	
	static class Root extends VFSFile
	{
		Root(File file)
		{
			
			

			String path = file.getPath();
			setPath(path);
			setDeletePath(path);
			setSymlinkPath(path);

			if(fsView.isFloppyDrive(file))
			{
				setType(VFSFile.FILESYSTEM);
				setName(path);
			}
			else if(fsView.isDrive(file))
			{
				setType(VFSFile.FILESYSTEM);
				setName(path + ' '
					+ fsView.getSystemDisplayName(file));
			}
			else if(file.isDirectory())
			{
				if(fsView.isFileSystemRoot(file))
					setType(VFSFile.DIRECTORY);
				else
					setType(VFSFile.FILESYSTEM);

				if(OperatingSystem.isMacOS())
					setName(MiscUtilities.getFileName(path));
				else
					setName(path);
			}
			else
				setType(VFSFile.FILE);
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
