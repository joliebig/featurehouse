
package org.gjt.sp.jedit.datatransfer;

import org.gjt.sp.jedit.io.FileVFS;
import org.gjt.sp.jedit.io.VFSFile;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;


public class ListVFSFileTransferable implements Transferable
{
	public static final DataFlavor jEditFileList = new DataFlavor(List.class, "application/x-java-jEdit-list-vfsfile");
	public static final DataFlavor[] supported = {jEditFileList, DataFlavor.stringFlavor, DataFlavor.javaFileListFlavor};
	
	private final List<VFSFile> files;

	public ListVFSFileTransferable(VFSFile[] files)
	{
		this.files = Collections.unmodifiableList(Arrays.asList(files));
	}

	public DataFlavor[] getTransferDataFlavors()
	{
		return supported;
	}

	public boolean isDataFlavorSupported(DataFlavor flavor)
	{
		return jEditFileList.equals(flavor) || DataFlavor.stringFlavor.equals(flavor);
	}

	public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException
	{
		if (jEditFileList.equals(flavor))
		{
			return files;
		}
		else if (DataFlavor.stringFlavor.equals(flavor))
		{
			StringBuilder builder = new StringBuilder();
			for (int i = 0; i < files.size(); i++)
			{
				VFSFile vfsFile = files.get(i);
				if (i != 0)
					builder.append('\n');
				builder.append(vfsFile);
			}
			return builder.toString();
		}
		else if (DataFlavor.javaFileListFlavor.equals(flavor))
		{
			List<File> files = new ArrayList<File>(this.files.size());
			for (VFSFile file : this.files)
			{
				if (file.getVFS() instanceof FileVFS)
					files.add(new File(file.getPath()));
			}
			return files;
		}
		throw new UnsupportedFlavorException(flavor);
	}
}
