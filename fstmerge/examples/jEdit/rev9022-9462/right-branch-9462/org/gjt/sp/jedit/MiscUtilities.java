

package org.gjt.sp.jedit;


import javax.swing.text.Segment;
import javax.swing.JMenuItem;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.jedit.io.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.ProgressObserver;
import org.gjt.sp.util.StandardUtilities;
import org.gjt.sp.util.IOUtilities;
import org.gjt.sp.util.XMLUtilities;
import org.gjt.sp.jedit.menu.EnhancedMenuItem;
import org.gjt.sp.jedit.bufferio.BufferIORequest;
import org.gjt.sp.jedit.buffer.JEditBuffer;



public class MiscUtilities
{
	
	public static final String UTF_8_Y = "UTF-8Y";

	

	
	
	public static String canonPath(String path)
	{
		if(path.length() == 0)
			return path;

		if(path.startsWith("file://"))
			path = path.substring("file://".length());
		else if(path.startsWith("file:"))
			path = path.substring("file:".length());
		else if(isURL(path))
			return path;

		if(File.separatorChar == '\\')
		{
				
				path = path.replace('/','\\');
				
				int trim = path.length();
				while(path.charAt(trim - 1) == ' ')
					trim--;

				if (path.charAt(trim - 1) == '\\')
					while (trim > 1 && path.charAt(trim - 2) == '\\')
					{
						trim--;
					}
				path = path.substring(0,trim);
		}
		else if(OperatingSystem.isMacOS())
		{
			
			path = path.replace(':','/');
		}

		if(path.startsWith('~' + File.separator))
		{
			path = path.substring(2);
			String home = System.getProperty("user.home");

			if(home.endsWith(File.separator))
				return home + path;
			else
				return home + File.separator + path;
		}
		else if(path.equals("~"))
			return System.getProperty("user.home");
		else
			return path;
	} 

	
	static final String varPatternString = "(\\$([a-zA-Z0-9_]+))";
	static final String varPatternString2 = "(\\$\\{([^}]+)\\})";
	static final Pattern varPattern = Pattern.compile(varPatternString);
	static final Pattern varPattern2 = Pattern.compile(varPatternString2);

	
	public static String expandVariables(String arg)
	{
		Pattern p = varPattern;
		Matcher m = p.matcher(arg);
		if (!m.find())
		{
			p = varPattern2;
			m = p.matcher(arg);
			if (!m.find()) 
				return arg;
		}
		String varName = m.group(2);
		String expansion = System.getenv(varName);
		if (expansion == null)
		{ 
			varName = varName.toUpperCase();
			String uparg = arg.toUpperCase();
			m = p.matcher(uparg);
			expansion = System.getenv(varName);
		}
		if (expansion != null)
		{
			expansion = expansion.replace("\\", "\\\\");
			return m.replaceFirst(expansion);
		}
		return arg;
	} 

	
	
	public static String resolveSymlinks(String path)
	{
		if(isURL(path))
			return path;

		
		if(OperatingSystem.isOS2())
			return path;
		
		
		if(OperatingSystem.isDOSDerived())
			{
				if(path.length() == 2 || path.length() == 3)
					{
						if(path.charAt(1) == ':')
							return path;
					}
			}
		try
			{
				return new File(path).getCanonicalPath();
			}
		catch(IOException io)
			{
				return path;
			}
	} 

	
	
	public static boolean isAbsolutePath(String path)
	{
		if(isURL(path))
			return true;
		else if(path.startsWith("~/") || path.startsWith("~" + File.separator) || path.equals("~"))
			return true;
		else if(OperatingSystem.isDOSDerived())
			{
				if(path.length() == 2 && path.charAt(1) == ':')
					return true;
				if(path.length() > 2 && path.charAt(1) == ':'
					&& (path.charAt(2) == '\\'
						|| path.charAt(2) == '/'))
					return true;
				if(path.startsWith("\\\\")
					|| path.startsWith("//"))
					return true;
			}
		
		else if(OperatingSystem.isUnix()
				|| OperatingSystem.isVMS())
			{
				
				if(path.length() > 0 && path.charAt(0) == '/')
					return true;
			}

		return false;
	} 

	
	
	public static String constructPath(String parent, String path)
	{
		if(isAbsolutePath(path))
			return canonPath(path);

		
		
		if(OperatingSystem.isDOSDerived())
			{
				if(path.length() == 2 && path.charAt(1) == ':')
					return path;
				else if(path.length() > 2 && path.charAt(1) == ':'
						&& path.charAt(2) != '\\')
					{
						path = path.substring(0,2) + '\\'
							+ path.substring(2);
						return canonPath(path);
					}
			}

		String dd = ".." + File.separator;
		String d = '.' + File.separator;

		if(parent == null)
			parent = System.getProperty("user.dir");

		for(;;)
			{
				if(path.equals("."))
					return parent;
				else if(path.equals(".."))
					return getParentOfPath(parent);
				else if(path.startsWith(dd) || path.startsWith("../"))
					{
						parent = getParentOfPath(parent);
						path = path.substring(3);
					}
				else if(path.startsWith(d) || path.startsWith("./"))
					path = path.substring(2);
				else
					break;
			}

		if(OperatingSystem.isDOSDerived()
			&& !isURL(parent)
		&& path.charAt(0) == '\\')
			parent = parent.substring(0,2);

		VFS vfs = VFSManager.getVFSForPath(parent);

		return canonPath(vfs.constructPath(parent,path));
	} 

	
	
	public static String constructPath(String parent,
				    String path1, String path2)
	{
		return constructPath(constructPath(parent,path1),path2);
	} 

	
	
	public static String concatPath(String parent, String path)
	{
		parent = canonPath(parent);
		path = canonPath(path);

		
		if (path.startsWith(File.separator))
			path = path.substring(1);
		else if ((path.length() >= 3) && (path.charAt(1) == ':'))
			path = path.replace(':', File.separatorChar);

		if (parent == null)
			parent = System.getProperty("user.dir");

		if (parent.endsWith(File.separator))
			return parent + path;
		else
			return parent + File.separator + path;
	} 

	
	
	public static int getFirstSeparatorIndex(String path)
	{
		int start = getPathStart(path);
		int index = path.indexOf('/',start);
		if(index == -1)
			index = path.indexOf(File.separatorChar,start);
		return index;
	} 

	
	
	public static int getLastSeparatorIndex(String path)
	{
		int start = getPathStart(path);
		if(start != 0)
			path = path.substring(start);
		int index = Math.max(path.lastIndexOf('/'),
							 path.lastIndexOf(File.separatorChar));
		if(index == -1)
			return index;
		else
			return index + start;
	} 

	
	
	public static String getFileExtension(String path)
	{
		int fsIndex = getLastSeparatorIndex(path);
		int index = path.indexOf('.',fsIndex);
		if(index == -1)
			return "";
		else
			return path.substring(index);
	} 

	
	
	public static String getFileName(String path)
	{
		return VFSManager.getVFSForPath(path).getFileName(path);
	} 

	
	
	public static String getFileNameNoExtension(String path)
	{
		String name = getFileName(path);
		int index = name.indexOf('.');
		if(index == -1)
			return name;
		else
			return name.substring(0,index);
	} 

	
	
	@Deprecated
	public static String getFileParent(String path)
	{
		return getParentOfPath(path);
	} 

	
	
	public static String getParentOfPath(String path)
	{
		return VFSManager.getVFSForPath(path).getParentOfPath(path);
	} 

	
	
	@Deprecated
	public static String getFileProtocol(String url)
	{
		return getProtocolOfURL(url);
	} 

	
	
	public static String getProtocolOfURL(String url)
	{
		return url.substring(0,url.indexOf(':'));
	} 

	
	
	public static boolean isURL(String str)
	{
		int fsIndex = getLastSeparatorIndex(str);
		if(fsIndex == 0) 
			return false;
		else if(fsIndex == 2) 
			return false;

		int cIndex = str.indexOf(':');
		if(cIndex <= 1) 
			return false;

		String protocol = str.substring(0,cIndex);
		VFS vfs = VFSManager.getVFSForProtocol(protocol);
		if(vfs != null && !(vfs instanceof UrlVFS))
			return true;

		try
			{
				new URL(str);
				return true;
			}
		catch(MalformedURLException mf)
			{
				return false;
			}
	} 

	
	
	public static void saveBackup(File file, int backups,
								  String backupPrefix, String backupSuffix,
								  String backupDirectory)
	{
		saveBackup(file,backups,backupPrefix,backupSuffix,backupDirectory,0);
	} 

	
	
	public static void saveBackup(File file, int backups,
			       String backupPrefix, String backupSuffix,
			       String backupDirectory, int backupTimeDistance)
	{
		if(backupPrefix == null)
			backupPrefix = "";
		if(backupSuffix == null)
			backupSuffix = "";

		String name = file.getName();

		
		if(backups == 1)
			{
				File backupFile = new File(backupDirectory,
										   backupPrefix + name + backupSuffix);
				long modTime = backupFile.lastModified();
				
				if(System.currentTimeMillis() - modTime
				   >= backupTimeDistance)
					{
						backupFile.delete();
						if (!file.renameTo(backupFile))
							IOUtilities.moveFile(file, backupFile);
					}
			}
		
		else
			{
				
				new File(backupDirectory,
						 backupPrefix + name + backupSuffix
						 + backups + backupSuffix).delete();

				File firstBackup = new File(backupDirectory,
											backupPrefix + name + backupSuffix
											+ "1" + backupSuffix);
				long modTime = firstBackup.lastModified();
				
				if(System.currentTimeMillis() - modTime
				   >= backupTimeDistance)
					{
						for(int i = backups - 1; i > 0; i--)
							{
								File backup = new File(backupDirectory,
													   backupPrefix + name
													   + backupSuffix + i
													   + backupSuffix);

								backup.renameTo(
												new File(backupDirectory,
														 backupPrefix + name
														 + backupSuffix + (i+1)
														 + backupSuffix));
							}

						File backupFile = new File(backupDirectory,
												   backupPrefix + name + backupSuffix
												   + "1" + backupSuffix);
						if (!file.renameTo(backupFile))
							IOUtilities.moveFile(file, backupFile);
					}
			}
	} 

	
	
	@Deprecated
	public static boolean moveFile(File source, File dest)
	{
		return IOUtilities.moveFile(source, dest);
	} 

	
	
	@Deprecated
	public static boolean copyStream(int bufferSize, ProgressObserver progress,
									 InputStream in, OutputStream out, boolean canStop)
		throws IOException
	{
		return IOUtilities.copyStream(bufferSize, progress, in, out, canStop);
	} 

	
	
	@Deprecated
	public static boolean copyStream(ProgressObserver progress, InputStream in, OutputStream out, boolean canStop)
		throws IOException
	{
		return IOUtilities.copyStream(4096,progress, in, out, canStop);
	} 

	
	
	public static boolean isBinary(Reader reader)
	throws IOException
	{
		int nbChars = jEdit.getIntegerProperty("vfs.binaryCheck.length",100);
		int authorized = jEdit.getIntegerProperty("vfs.binaryCheck.count",1);
		for (long i = 0L;i < nbChars;i++)
		{
			int c = reader.read();
			if (c == -1)
				return false;
			if (c == 0)
			{
				authorized--;
				if (authorized == 0)
					return true;
			}
		}
		return false;
	} 

	
	
	public static boolean isBackup( String filename ) {
		if (filename.startsWith("#")) return true;
		if (filename.endsWith("~")) return true;
		if (filename.endsWith(".bak")) return true;
		return false;
	} 


	
	
	public static Reader autodetect(InputStream in, Buffer buffer) throws IOException
	{
		in = new BufferedInputStream(in,
				BufferIORequest.getByteIOBufferSize());

		String encoding;
		if (buffer == null)
			encoding = System.getProperty("file.encoding");
		else
			encoding = buffer.getStringProperty(JEditBuffer.ENCODING);

		if(!in.markSupported())
			Log.log(Log.WARNING,MiscUtilities.class,"Mark not supported: " + in);
		else if(buffer == null || buffer.getBooleanProperty(Buffer.ENCODING_AUTODETECT))
		{
			in.mark(BufferIORequest.XML_PI_LENGTH);
			int b1 = in.read();
			int b2 = in.read();
			int b3 = in.read();

			if(b1 == BufferIORequest.GZIP_MAGIC_1 && b2 == BufferIORequest.GZIP_MAGIC_2)
			{
				in.reset();
				Log.log(Log.DEBUG, MiscUtilities.class, "Stream is Gzipped");
				in = new GZIPInputStream(in);
				if (buffer != null)
					buffer.setBooleanProperty(Buffer.GZIPPED,true);
				
				return autodetect(in, buffer);
			}
			else if (b1 == BufferIORequest.UNICODE_MAGIC_1
				&& b2 == BufferIORequest.UNICODE_MAGIC_2)
			{
				in.reset();
				in.read();
				in.read();
				encoding = "UTF-16";
				if (buffer != null)
				{
					buffer.setProperty(JEditBuffer.ENCODING,encoding);
					buffer.setProperty(BufferIORequest.BOM_PROP,BufferIORequest.UTF_BOM.BE);
				}
			}
			else if (b1 == BufferIORequest.UNICODE_MAGIC_2
				&& b2 == BufferIORequest.UNICODE_MAGIC_1)
			{
				in.reset();
				in.read();
				in.read();
				encoding = "UTF-16";
				if (buffer != null)
				{
					buffer.setProperty(JEditBuffer.ENCODING,encoding);
					buffer.setProperty(BufferIORequest.BOM_PROP,BufferIORequest.UTF_BOM.LE);
				}
			}
			else if(b1 == BufferIORequest.UTF8_MAGIC_1 && b2 == BufferIORequest.UTF8_MAGIC_2
				&& b3 == BufferIORequest.UTF8_MAGIC_3)
			{
				
				
				if (buffer != null)
					buffer.setProperty(JEditBuffer.ENCODING, MiscUtilities.UTF_8_Y);

				encoding = "UTF-8";
			}
			else
			{
				in.reset();

				byte[] _xmlPI = new byte[BufferIORequest.XML_PI_LENGTH];
				int offset = 0;
				int count;
				while((count = in.read(_xmlPI,offset,
					BufferIORequest.XML_PI_LENGTH - offset)) != -1)
				{
					offset += count;
					if(offset == BufferIORequest.XML_PI_LENGTH)
						break;
				}

				String xmlEncoding = getXMLEncoding(new String(
					_xmlPI,0,offset,"ASCII"));

				if (xmlEncoding == null)
				{

					if (Options.X_AUTODETECT)
					{
						in.reset();
						String coding = xAutodetect(in);
						if (coding != null)
							encoding = coding;
					}
				}
				else
				{
					encoding = xmlEncoding;
					if (buffer != null)
						buffer.setProperty(JEditBuffer.ENCODING,encoding);
				}

				if(encoding.equals(MiscUtilities.UTF_8_Y))
					encoding = "UTF-8";

				in.reset();
			}
		}
		Log.log(Log.DEBUG, MiscUtilities.class, "Stream encoding detected is " + encoding);
		return new InputStreamReader(in, encoding);
	} 

	
	private static String xAutodetect(InputStream in) throws IOException
	{
		BufferedReader reader = new BufferedReader(new InputStreamReader(in));
		int i = 0;
		while (i < 10)
		{
			i++;
			String line = reader.readLine();
			if (line == null)
				return null;
			int pos = line.indexOf(":encoding=");
			if (pos != -1)
			{
				int p2 = line.indexOf(':', pos + 10);
				String encoding = line.substring(pos + 10, p2);
				return encoding;
			}
		}
		return null;
	} 

	
	
	private static String getXMLEncoding(String xmlPI)
	{
		if(!xmlPI.startsWith("<?xml"))
			return null;

		int index = xmlPI.indexOf("encoding=");
		if(index == -1 || index + 9 == xmlPI.length())
			return null;

		char ch = xmlPI.charAt(index + 9);
		int endIndex = xmlPI.indexOf(ch,index + 10);
		if(endIndex == -1)
			return null;

		String encoding = xmlPI.substring(index + 10,endIndex);

		if(Charset.isSupported(encoding))
			return encoding;
		else
		{
			Log.log(Log.WARNING,MiscUtilities.class,"XML PI specifies "
				+ "unsupported encoding: " + encoding);
			return null;
		}
	} 

	
	
	@Deprecated
	public static void closeQuietly(InputStream in)
	{
		IOUtilities.closeQuietly(in);
	} 

	
	
	@Deprecated
	public static void closeQuietly(OutputStream out)
	{
		IOUtilities.closeQuietly(out);
	} 

	
	
	public static String fileToClass(String name)
	{
		char[] clsName = name.toCharArray();
		for(int i = clsName.length - 6; i >= 0; i--)
			if(clsName[i] == '/')
				clsName[i] = '.';
		return new String(clsName,0,clsName.length - 6);
	} 

	
	
	public static String classToFile(String name)
	{
		return name.replace('.','/').concat(".class");
	} 

	
	
	public static boolean pathsEqual(String p1, String p2)
	{
		VFS v1 = VFSManager.getVFSForPath(p1);
		VFS v2 = VFSManager.getVFSForPath(p2);

		if(v1 != v2)
			return false;

		if(p1.endsWith("/") || p1.endsWith(File.separator))
			p1 = p1.substring(0,p1.length() - 1);

		if(p2.endsWith("/") || p2.endsWith(File.separator))
			p2 = p2.substring(0,p2.length() - 1);

		if((v1.getCapabilities() & VFS.CASE_INSENSITIVE_CAP) != 0)
			return p1.equalsIgnoreCase(p2);
		else
			return p1.equals(p2);
	} 

	

	

	
	
	@Deprecated
	public static int getLeadingWhiteSpace(String str)
	{
		return StandardUtilities.getLeadingWhiteSpace(str);
	} 

	
	
	@Deprecated
	public static int getTrailingWhiteSpace(String str)
	{
		return StandardUtilities.getTrailingWhiteSpace(str);
	} 

	
	
	@Deprecated
	public static int getLeadingWhiteSpaceWidth(String str, int tabSize)
	{
		return StandardUtilities.getLeadingWhiteSpaceWidth(str, tabSize);
	} 

	
	
	@Deprecated
	public static int getVirtualWidth(Segment seg, int tabSize)
	{
		return StandardUtilities.getVirtualWidth(seg, tabSize);
	} 

	
	
	@Deprecated
	public static int getOffsetOfVirtualColumn(Segment seg, int tabSize,
					    int column, int[] totalVirtualWidth)
	{
		return StandardUtilities.getOffsetOfVirtualColumn(seg, tabSize, column, totalVirtualWidth);
	} 

	
	
	@Deprecated
	public static String createWhiteSpace(int len, int tabSize)
	{
		return StandardUtilities.createWhiteSpace(len,tabSize,0);
	} 

	
	
	@Deprecated
	public static String createWhiteSpace(int len, int tabSize, int start)
	{
		return StandardUtilities.createWhiteSpace(len, tabSize, start);
	} 

	
	
	@Deprecated
	public static String globToRE(String glob)
	{
		return StandardUtilities.globToRE(glob);
	} 

	
	
	public static String escapesToChars(String str)
	{
		StringBuffer buf = new StringBuffer();
		for(int i = 0; i < str.length(); i++)
		{
			char c = str.charAt(i);
			switch(c)
			{
			case '\\':
				if(i == str.length() - 1)
				{
					buf.append('\\');
					break;
				}
				c = str.charAt(++i);
				switch(c)
				{
				case 'n':
					buf.append('\n');
					break;
				case 't':
					buf.append('\t');
					break;
				default:
					buf.append(c);
					break;
				}
				break;
			default:
				buf.append(c);
			}
		}
		return buf.toString();
	} 

	
	
	public static String charsToEscapes(String str)
	{
		return charsToEscapes(str,"\n\t\\\"'");
	} 

	
	
	public static String charsToEscapes(String str, String toEscape)
	{
		StringBuffer buf = new StringBuffer();
		for(int i = 0; i < str.length(); i++)
		{
			char c = str.charAt(i);
			if(toEscape.indexOf(c) != -1)
			{
				if(c == '\n')
					buf.append("\\n");
				else if(c == '\t')
					buf.append("\\t");
				else
				{
					buf.append('\\');
					buf.append(c);
				}
			}
			else
				buf.append(c);
		}
		return buf.toString();
	} 

	
	
	@Deprecated
	public static int compareVersions(String v1, String v2)
	{
		return StandardUtilities.compareStrings(v1,v2,false);
	} 

	
	
	@Deprecated
	public static int compareStrings(String str1, String str2, boolean ignoreCase)
	{
		return StandardUtilities.compareStrings(str1, str2, ignoreCase);
	} 

	
	
	@Deprecated
	public static boolean stringsEqual(String s1, String s2)
	{
		return org.gjt.sp.util.StandardUtilities.objectsEqual(s1,s2);
	} 

	
	
	@Deprecated
	public static boolean objectsEqual(Object o1, Object o2)
	{
		return StandardUtilities.objectsEqual(o1, o2);
	} 

	
	
	@Deprecated
	public static String charsToEntities(String str)
	{
		return XMLUtilities.charsToEntities(str,false);
	} 

	
	public static final DecimalFormat KB_FORMAT = new DecimalFormat("#.# KB");
	public static final DecimalFormat MB_FORMAT = new DecimalFormat("#.# MB");

	
	public static String formatFileSize(long length)
	{
		if(length < 1024)
			return length + " bytes";
		else if(length < 1024 << 10)
			return KB_FORMAT.format((double)length / 1024);
		else
			return MB_FORMAT.format((double)length / 1024 / 1024);
	} 

	
	
	public static String getLongestPrefix(List str, boolean ignoreCase)
	{
		if(str.size() == 0)
			return "";

		int prefixLength = 0;

loop:		for(;;)
		{
			String s = str.get(0).toString();
			if(prefixLength >= s.length())
				break loop;
			char ch = s.charAt(prefixLength);
			for(int i = 1; i < str.size(); i++)
			{
				s = str.get(i).toString();
				if(prefixLength >= s.length())
					break loop;
				if(!compareChars(s.charAt(prefixLength),ch,ignoreCase))
					break loop;
			}
			prefixLength++;
		}

		return str.get(0).toString().substring(0,prefixLength);
	} 

	
	
	public static String getLongestPrefix(String[] str, boolean ignoreCase)
	{
		return getLongestPrefix((Object[])str,ignoreCase);
	} 

	
	
	public static String getLongestPrefix(Object[] str, boolean ignoreCase)
	{
		if(str.length == 0)
			return "";

		int prefixLength = 0;

		String first = str[0].toString();

loop:		for(;;)
		{
			if(prefixLength >= first.length())
				break loop;
			char ch = first.charAt(prefixLength);
			for(int i = 1; i < str.length; i++)
			{
				String s = str[i].toString();
				if(prefixLength >= s.length())
					break loop;
				if(!compareChars(s.charAt(prefixLength),ch,ignoreCase))
					break loop;
			}
			prefixLength++;
		}

		return first.substring(0,prefixLength);
	} 

	

	
	
	@Deprecated
	public static void quicksort(Object[] obj, Comparator compare)
	{
		Arrays.sort(obj,compare);
	}


	
	@Deprecated
	public static void quicksort(Vector vector, Comparator compare)
	{
		Collections.sort(vector,compare);
	}

	
	@Deprecated
	public static void quicksort(List list, Comparator compare)
	{
		Collections.sort(list,compare);
	}

	
	@Deprecated
	public static void quicksort(Object[] obj, Compare compare)
	{
		Arrays.sort(obj,compare);
	}

	
	@Deprecated
	public static void quicksort(Vector vector, Compare compare)
	{
		Collections.sort(vector,compare);
	} 

	
	
	@Deprecated
	public interface Compare extends Comparator
	{
		int compare(Object obj1, Object obj2);
	} 

	
	
	@Deprecated
	public static class StringCompare implements Compare
	{
		public int compare(Object obj1, Object obj2)
		{
			return StandardUtilities.compareStrings(obj1.toString(),
				obj2.toString(),false);
		}
	} 

	
	
	public static class StringICaseCompare implements Comparator<Object>
	{
		public int compare(Object obj1, Object obj2)
		{
			return StandardUtilities.compareStrings(obj1.toString(), obj2.toString(), true);
		}
	} 

	
	
	public static class MenuItemCompare implements Compare
	{
		public int compare(Object obj1, Object obj2)
		{
			boolean obj1E, obj2E;
			obj1E = obj1 instanceof EnhancedMenuItem;
			obj2E = obj2 instanceof EnhancedMenuItem;
			if(obj1E && !obj2E)
				return 1;
			else if(obj2E && !obj1E)
				return -1;
			else
				return StandardUtilities.compareStrings(((JMenuItem)obj1).getText(),
					((JMenuItem)obj2).getText(),true);
		}
	} 

	
	
	public static String buildToVersion(String build)
	{
		if(build.length() != 11)
			return "<unknown version: " + build + ">";
		
		int major = Integer.parseInt(build.substring(0,2));
		
		int minor = Integer.parseInt(build.substring(3,5));
		
		int beta = Integer.parseInt(build.substring(6,8));
		
		int bugfix = Integer.parseInt(build.substring(9,11));

		return major + "." + minor
			+ (beta != 99 ? "pre" + beta :
			(bugfix != 0 ? "." + bugfix : "final"));
	} 

	
	
	public static boolean isToolsJarAvailable()
	{
		Log.log(Log.DEBUG, MiscUtilities.class,"Searching for tools.jar...");

		Vector paths = new Vector();

		
		paths.addElement("System classpath: "
			+ System.getProperty("java.class.path"));

		try
		{
			
			
			try
			{
				Class.forName("sun.tools.javac.Main");
			}
			catch(ClassNotFoundException e1)
			{
				Class.forName("com.sun.tools.javac.Main");
			}
			Log.log(Log.DEBUG, MiscUtilities.class,
				"- is in classpath. Fine.");
			return true;
		}
		catch(ClassNotFoundException e)
		{
			
			
		} 

		
		String settingsDir = jEdit.getSettingsDirectory();
		if(settingsDir != null)
		{
			String toolsPath = constructPath(settingsDir, "jars",
				"tools.jar");
			paths.addElement(toolsPath);
			if(new File(toolsPath).exists())
			{
				Log.log(Log.DEBUG, MiscUtilities.class,
					"- is in the user's jars folder. Fine.");
				
				return true;
			}
		} 

		
		String jEditDir = jEdit.getJEditHome();
		if(jEditDir != null)
		{
			String toolsPath = constructPath(jEditDir, "jars", "tools.jar");
			paths.addElement(toolsPath);
			if(new File(toolsPath).exists())
			{
				Log.log(Log.DEBUG, MiscUtilities.class,
					"- is in jEdit's system jars folder. Fine.");
				
				return true;
			}
		} 

		
		String toolsPath = System.getProperty("java.home");
		if(toolsPath.toLowerCase().endsWith(File.separator + "jre"))
			toolsPath = toolsPath.substring(0, toolsPath.length() - 4);
		toolsPath = constructPath(toolsPath, "lib", "tools.jar");
		paths.addElement(toolsPath);

		if(!(new File(toolsPath).exists()))
		{
			Log.log(Log.WARNING, MiscUtilities.class,
				"Could not find tools.jar.\n"
				+ "I checked the following locations:\n"
				+ paths.toString());
			return false;
		} 

		
		PluginJAR jar = jEdit.getPluginJAR(toolsPath);
		if(jar == null)
		{
			Log.log(Log.DEBUG, MiscUtilities.class,
				"- adding " + toolsPath + " to jEdit plugins.");
			jEdit.addPluginJAR(toolsPath);
		}
		else
			Log.log(Log.DEBUG, MiscUtilities.class,
				"- has been loaded before.");
		

		return true;
	} 

	
	
	public static int parsePermissions(String s)
	{
		int permissions = 0;

		if(s.length() == 9)
		{
			if(s.charAt(0) == 'r')
				permissions += 0400;
			if(s.charAt(1) == 'w')
				permissions += 0200;
			if(s.charAt(2) == 'x')
				permissions += 0100;
			else if(s.charAt(2) == 's')
				permissions += 04100;
			else if(s.charAt(2) == 'S')
				permissions += 04000;
			if(s.charAt(3) == 'r')
				permissions += 040;
			if(s.charAt(4) == 'w')
				permissions += 020;
			if(s.charAt(5) == 'x')
				permissions += 010;
			else if(s.charAt(5) == 's')
				permissions += 02010;
			else if(s.charAt(5) == 'S')
				permissions += 02000;
			if(s.charAt(6) == 'r')
				permissions += 04;
			if(s.charAt(7) == 'w')
				permissions += 02;
			if(s.charAt(8) == 'x')
				permissions += 01;
			else if(s.charAt(8) == 't')
				permissions += 01001;
			else if(s.charAt(8) == 'T')
				permissions += 01000;
		}

		return permissions;
	} 

	
	
	@Deprecated
	public static String[] getEncodings()
	{
		return getEncodings(false);
	} 

	
	
	public static String[] getEncodings(boolean getSelected)
	{
		List returnValue = new ArrayList();

		Map map = Charset.availableCharsets();
		Iterator iter = map.keySet().iterator();

		if ((getSelected && !jEdit.getBooleanProperty("encoding.opt-out."+UTF_8_Y,false)) ||
			!getSelected)
		{
			returnValue.add(UTF_8_Y);
		}

		while(iter.hasNext())
		{
			String encoding = (String)iter.next();
			if ((getSelected && !jEdit.getBooleanProperty("encoding.opt-out."+encoding,false)) ||
				!getSelected)
			{
				returnValue.add(encoding);
			}
		}

		return (String[])returnValue.toArray(
			new String[returnValue.size()]);
	} 

	
	
	public static String throwableToString(Throwable t)
	{
		StringWriter s = new StringWriter();
		t.printStackTrace(new PrintWriter(s));
		return s.toString();
	} 

	
	
	@Deprecated
	public static boolean parseXML(InputStream in, DefaultHandler handler)
		throws IOException
	{
		return XMLUtilities.parseXML(in, handler);
	} 

	
	
	@Deprecated
	public static InputSource findEntity(String systemId, String test, Class where)
	{
		return XMLUtilities.findEntity(systemId, test, where);
	} 

	
	private MiscUtilities() {}

	
	
	private static boolean compareChars(char ch1, char ch2, boolean ignoreCase)
	{
		if(ignoreCase)
			return Character.toUpperCase(ch1) == Character.toUpperCase(ch2);
		else
			return ch1 == ch2;
	} 

	
	private static int getPathStart(String path)
	{
		int start = 0;
		if(path.startsWith("/"))
			return 1;
		else if(OperatingSystem.isDOSDerived()
			&& path.length() >= 3
			&& path.charAt(1) == ':'
			&& (path.charAt(2) == '/'
			|| path.charAt(2) == '\\'))
			return 3;
		else
			return 0;
	} 

	
}
