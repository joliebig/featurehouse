

package org.gjt.sp.jedit;


import javax.swing.text.Segment;
import javax.swing.JMenuItem;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.MalformedInputException;
import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.jedit.io.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.ProgressObserver;
import org.gjt.sp.util.StandardUtilities;
import org.gjt.sp.util.IOUtilities;

import org.gjt.sp.util.XMLUtilities;
import org.gjt.sp.jedit.menu.MenuItemTextComparator;
import org.gjt.sp.jedit.buffer.JEditBuffer;



public class MiscUtilities
{
	
	@Deprecated public static final String UTF_8_Y = "UTF-8Y";

	

	
	
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
		else if("~".equals(path))
			return System.getProperty("user.home");
		else if ("-".equals(path))
			return getParentOfPath(jEdit.getActiveView().getBuffer().getPath());
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

	
	
	public static String abbreviate(String path)
	{
		if (svc == null)
			svc = new VarCompressor();
		return svc.compress(path);
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
		else if(path.startsWith("~/") || path.startsWith('~' + File.separator) || "~".equals(path))
			return true;
		else if ("-".equals(path))
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

		if (parent == null)
			parent = System.getProperty("user.dir");

		if (path == null || path.length() == 0)
			return parent;

		
		
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

		for(;;)
		{
			if(".".equals(path))
				return parent;
			else if("..".equals(path))
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
		if(path.length() == 0)
			return parent;

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
		else if (path.length() >= 3 && path.charAt(1) == ':')
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
		int index = Math.max(
			path.lastIndexOf('/'), path.lastIndexOf(File.separatorChar));
		if(index == -1)
			return index;
		else
			return index + start;
	} 


	
	
	public static String getFileExtension(String path)
	{
		int fsIndex = getLastSeparatorIndex(path);
		int index = path.lastIndexOf('.');
		
		if(index == -1 || index < fsIndex )
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
				Log.log(Log.DEBUG,MiscUtilities.class,
					"Saving backup of file \"" +
					file.getAbsolutePath() + "\" to \"" +
					backupFile.getAbsolutePath() + '"');
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
				+ '1' + backupSuffix);
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

					backup.renameTo(new File(backupDirectory,
						backupPrefix + name
						+ backupSuffix + (i + 1)
						+ backupSuffix));
				}

				File backupFile = new File(backupDirectory,
					backupPrefix + name + backupSuffix
					+ '1' + backupSuffix);
				Log.log(Log.DEBUG,MiscUtilities.class,
					"Saving backup of file \"" +
					file.getAbsolutePath() + "\" to \"" +
					backupFile.getAbsolutePath() + '"');
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

	
	
	@Deprecated
	public static boolean isBinary(Reader reader) throws IOException
	{
		return containsNullCharacter(reader);
	}

	
	public static boolean isBinary(InputStream in) throws IOException
	{
		AutoDetection.Result detection = new AutoDetection.Result(in);
		
		if (detection.getDetectedEncoding() != null)
		{
			return false;
		}
		
		
		try
		{
			return containsNullCharacter(
				new InputStreamReader(detection.getRewindedStream()));
		}
		catch (MalformedInputException mie)
		{
			
			return true;
		}
	} 

	
	
	public static boolean isBackup(String filename)
	{
		if (filename.startsWith("#")) return true;
		if (filename.endsWith("~")) return true;
		if (filename.endsWith(".bak")) return true;
		return false;
	} 


	
	
	public static Reader autodetect(InputStream in, Buffer buffer) throws IOException
	{
		String encoding;
		if (buffer == null)
			encoding = System.getProperty("file.encoding");
		else
			encoding = buffer.getStringProperty(JEditBuffer.ENCODING);
		boolean gzipped = false;

		if (buffer == null || buffer.getBooleanProperty(Buffer.ENCODING_AUTODETECT))
		{
			AutoDetection.Result detection = new AutoDetection.Result(in);
			gzipped = detection.streamIsGzipped();
			if (gzipped)
			{
				Log.log(Log.DEBUG, MiscUtilities.class
					, "Stream is Gzipped");
			}
			String detected = detection.getDetectedEncoding();
			if (detected != null)
			{
				encoding = detected;
				Log.log(Log.DEBUG, MiscUtilities.class
					, "Stream encoding detected is " + detected);
			}
			in = detection.getRewindedStream();
		}
		else
		{
			
			in = AutoDetection.getMarkedStream(in);
		}

		Reader result = EncodingServer.getTextReader(in, encoding);
		if (buffer != null)
		{
			
			if (gzipped)
			{
				buffer.setBooleanProperty(Buffer.GZIPPED,true);
			}
			buffer.setProperty(JEditBuffer.ENCODING, encoding);
		}
		return result;
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
		StringBuilder buf = new StringBuilder();
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

	
	
	@Deprecated
	public static String charsToEscapes(String str)
	{
		return StandardUtilities.charsToEscapes(str);
	}

	
	@Deprecated
	public static String charsToEscapes(String str, String toEscape)
	{
		return StandardUtilities.charsToEscapes(str, toEscape);
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
		return StandardUtilities.objectsEqual(s1,s2);
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

	
	public static final DecimalFormat KB_FORMAT = new DecimalFormat("#.# kB");
	public static final DecimalFormat MB_FORMAT = new DecimalFormat("#.# MB");

	
	public static String formatFileSize(long length)
	{
		if(length < 1024)
		{
			return length + " Bytes";
		}
		else if(length < 1024 << 10)
		{
			return KB_FORMAT.format((double)length / 1024);
		}
		else
		{
			return MB_FORMAT.format((double)length / 1024 / 1024);
		}
	} 

	
	
	public static String getLongestPrefix(List<String> str, boolean ignoreCase)
	{
		if(str.isEmpty())
			return "";

		int prefixLength = 0;

loop:		for(;;)
		{
			String s = str.get(0);
			if(prefixLength >= s.length())
				break loop;
			char ch = s.charAt(prefixLength);
			for(int i = 1; i < str.size(); i++)
			{
				s = str.get(i);
				if(prefixLength >= s.length())
					break loop;
				if(!compareChars(s.charAt(prefixLength),ch,ignoreCase))
					break loop;
			}
			prefixLength++;
		}

		return str.get(0).substring(0,prefixLength);
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

	
	
	@Deprecated
	public static class StringICaseCompare implements Comparator<Object>
	{
		public int compare(Object obj1, Object obj2)
		{
			return StandardUtilities.compareStrings(obj1.toString(), obj2.toString(), true);
		}
	} 

	
	
	@Deprecated
	public static class MenuItemCompare implements Compare
	{
		private MenuItemTextComparator comparator = new MenuItemTextComparator();

		public int compare(Object obj1, Object obj2)
		{
			return comparator.compare((JMenuItem)obj1, (JMenuItem)obj2);
		}
	} 

	
	
	public static String buildToVersion(String build)
	{
		if(build.length() != 11)
			return "<unknown version: " + build + '>';
		
		int major = Integer.parseInt(build.substring(0,2));
		
		int minor = Integer.parseInt(build.substring(3,5));
		
		int beta = Integer.parseInt(build.substring(6,8));
		
		int bugfix = Integer.parseInt(build.substring(9,11));

		return major + "." + minor
			+ (beta != 99 ? "pre" + beta :
			(bugfix != 0 ? "." + bugfix : ""));
	} 

	
	
	public static boolean isToolsJarAvailable()
	{
		Log.log(Log.DEBUG, MiscUtilities.class,"Searching for tools.jar...");

		Collection<String> paths = new LinkedList<String>();

		
		paths.add("System classpath: "
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
			paths.add(toolsPath);
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
			paths.add(toolsPath);
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
		paths.add(toolsPath);

		if(!new File(toolsPath).exists())
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
		Set<String> set;
		if (getSelected)
		{
			set = EncodingServer.getSelectedNames();
		}
		else
		{
			set = EncodingServer.getAvailableNames();
		}
		return set.toArray(new String[set.size()]);
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
		if(path.startsWith("/"))
			return 0;
		else if(OperatingSystem.isDOSDerived()
			&& path.length() >= 3
			&& path.charAt(1) == ':'
			&& (path.charAt(2) == '/'
			|| path.charAt(2) == '\\'))
			return 3;
		else
			return 0;
	} 

	
	private static boolean containsNullCharacter(Reader reader)
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

	

	static VarCompressor svc = null;

	
	
	static class VarCompressor
	{
		
		final Map<String, String> prefixMap = new HashMap<String, String>();
		
		final Map<String, String> previous = new HashMap<String, String>();

		
		VarCompressor()
		{
			ProcessBuilder pb = new ProcessBuilder();
			Map<String, String> env = pb.environment();
			if (OperatingSystem.isUnix())
				prefixMap.put(System.getProperty("user.home"), "~");
			for (Map.Entry<String, String> entry: env.entrySet())
			{
				String k = entry.getKey();
				if (k.equalsIgnoreCase("pwd") || k.equalsIgnoreCase("oldpwd")) continue;
				if (!Character.isLetter(k.charAt(0))) continue;
				String v = entry.getValue();
				
				if (!canBePathPrefix(v)) continue;
				
				if (v.endsWith(File.separator))
					v = v.substring(0, v.length()-1);
				
				if (OperatingSystem.isWindows())
					if (k.length()+2 > v.length()) continue; 
				else
					if (k.length()+1 > v.length()) continue; 
				if (OperatingSystem.isWindows())
				{
					
					v = v.toLowerCase();
					k = k.toLowerCase();
				}
				if (prefixMap.containsKey(v))
				{
					String otherKey = prefixMap.get(v);
					if (otherKey.length() < k.length()) continue;
				}
				prefixMap.put(v, k);
			}
		} 

		
		String compress(String path)
		{
			String original = path;
			if (previous.containsKey(path))
			{
				return previous.get(path);
			}
			String bestPrefix = "/";
			String verifiedPrefix = bestPrefix;
			for (String tryPrefix : prefixMap.keySet())
			{
				if (tryPrefix.length() < bestPrefix.length()) continue;
				if (OperatingSystem.isWindows() &&
				    path.toLowerCase().startsWith(tryPrefix))
					bestPrefix = tryPrefix;
				else if (path.startsWith(tryPrefix))
				{
					bestPrefix = tryPrefix;
				}
				
				if (!bestPrefix.equals(verifiedPrefix))
				{
					String remainder = original.substring(bestPrefix.length());
					if (remainder.length() < 1 || remainder.startsWith(File.separator))
						verifiedPrefix = bestPrefix;
					else bestPrefix = verifiedPrefix;
				}
			}
			if (bestPrefix.length() > 1)
			{
				String remainder = original.substring(bestPrefix.length());
				String envvar = prefixMap.get(bestPrefix);
				if (envvar.equals("~"))
					path = envvar + remainder;
				else if (OperatingSystem.isWindows())
					path = '%' + envvar.toUpperCase() + '%' + remainder;
				else
					path = '$' + envvar + remainder;
			}
			previous.put(original, path);
			return path;
		} 

		
		
		
		private boolean canBePathPrefix(String s)
		{
			
			
			
			return !s.contains(File.pathSeparator)
				&& new File(s).isAbsolute();
		} 
	} 

}
