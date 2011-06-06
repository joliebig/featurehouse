

package org.gjt.sp.util;

import java.io.*;


public class IOUtilities
{
	
	
	public static boolean moveFile(File source, File dest)
	{
		boolean ok = false;

		if ((dest.exists() && dest.canWrite())
			|| (!dest.exists() && dest.getParentFile().canWrite()))
			{
				OutputStream fos = null;
				InputStream fis = null;
				try
				{
					fos = new FileOutputStream(dest);
					fis = new FileInputStream(source);
					ok = copyStream(32768,null,fis,fos,false);
				}
				catch (IOException ioe)
				{
					Log.log(Log.WARNING, IOUtilities.class,
							"Error moving file: " + ioe + " : " + ioe.getMessage());
				}
				finally
				{
					closeQuietly(fos);
					closeQuietly(fis);
				}

				if(ok)
					source.delete();
			}
		return ok;
	} 

	
	
	public static boolean copyStream(int bufferSize, ProgressObserver progress,
					InputStream in, OutputStream out, boolean canStop)
		throws IOException
	{
		byte[] buffer = new byte[bufferSize];
		int n;
		long copied = 0L;
		while (-1 != (n = in.read(buffer)))
		{
			out.write(buffer, 0, n);
			copied += n;
			if(progress != null)
				progress.setValue(copied);
			if(canStop && Thread.interrupted()) return false;
		}
		return true;
	}

	
	public static boolean copyStream(ProgressObserver progress,
					 InputStream in, OutputStream out, boolean canStop)
		throws IOException
	{
		return copyStream(4096,progress, in, out, canStop);
	} 

	
	
	public static long fileLength(File file)
	{
		long length = 0L;
		if (file.isFile())
			length = file.length();
		else if (file.isDirectory())
		{
			File[] files = file.listFiles();
			for (int i = 0; i < files.length; i++)
			{
				length += fileLength(files[i]);
			}
		}
		return length;
	} 

	
	
	public static void closeQuietly(InputStream in)
	{
		if(in != null)
		{
			try
			{
				in.close();
			}
			catch (IOException e)
			{
				
			}
		}
	}

	
	public static void closeQuietly(OutputStream out)
	{
		if(out != null)
		{
			try {
				if (out instanceof Flushable)
				{
					((Flushable)out).flush();
				}
			}
			catch (IOException e)
			{
				
			}
			try
			{
				out.close();
			}
			catch (IOException e)
			{
				
			}
		}
	}

	
	public static void closeQuietly(Reader r)
	{
		if(r != null)
		{
			try
			{
				r.close();
			}
			catch (IOException e)
			{
				
			}
		}
	}

	
	public static void closeQuietly(Writer out)
	{
		if(out != null)
		{
			try {
				if (out instanceof Flushable)
				{
					((Flushable)out).flush();
				}
			}
			catch (IOException e)
			{
				
			}
			try
			{
				out.close();
			}
			catch (IOException e)
			{
				
			}
		}
	}

	
	public static void closeQuietly(Closeable closeable)
	{
		if(closeable != null)
		{
			try {
				if (closeable instanceof Flushable)
				{
					((Flushable)closeable).flush();
				}
			}
			catch (IOException e)
			{
				
			}
			try
			{
				closeable.close();
			}
			catch (IOException e)
			{
				
			}
		}
	} 

	
	private IOUtilities()
	{
	} 
}
