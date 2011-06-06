

package org.gjt.sp.jedit;


import javax.swing.text.Segment;
import javax.swing.JMenuItem;
import java.io.*;
import java.text.DecimalFormat;
import java.util.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.util.Log;



public class MiscUtilities
{
	

	
	
	public static String canonPath(String path)
	{
		if(path.startsWith("file://"))
			path = path.substring("file://".length());
		else if(path.startsWith("file:"))
			path = path.substring("file:".length());
		else if(isURL(path))
			return path;

		if(File.separatorChar == '\\')
		{
			
			path = path.replace('/','\\');
		}

		if(path.startsWith("~" + File.separator))
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

	
	
	public static String resolveSymlinks(String path)
	{
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
				&& path.charAt(2) == '\\')
				return true;
			if(path.startsWith("\\\\"))
				return true;
		}
		else if(OperatingSystem.isUnix())
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
		String d = "." + File.separator;

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
			else if(path.startsWith(d))
				path = path.substring(2);
			else
				break;
		}

		if(OperatingSystem.isDOSDerived() && path.startsWith("\\"))
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

	
	
	public static String getFileExtension(String name)
	{
		int index = name.indexOf('.');
		if(index == -1)
			return "";
		else
			return name.substring(index);
	} 

	
	
	public static String getFileName(String path)
	{
		return VFSManager.getVFSForPath(path).getFileName(path);
	} 

	
	
	public static String getFileNameNoExtension(String path)
	{
		String name = getFileName(path);
		int index = name.lastIndexOf('.');
		if(index == -1)
			return name;
		else
			return name.substring(0,index);
	} 

	
	
	public static String getFileParent(String path)
	{
		return getParentOfPath(path);
	} 

	
	
	public static String getParentOfPath(String path)
	{
		return VFSManager.getVFSForPath(path).getParentOfPath(path);
	} 

	
	
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
		int fsIndex = Math.max(str.indexOf(File.separatorChar),
			str.indexOf('/'));
		if(fsIndex == 0) 
			return false;
		else if(fsIndex == 2) 
			return false;

		int cIndex = str.indexOf(':');
		if(cIndex <= 1) 
			return false;
		else if(fsIndex != -1 && cIndex > fsIndex) 
			return false;

		return true;
	} 

	
	
	public static void saveBackup(File file, int backups,
		String backupPrefix, String backupSuffix,
		String backupDirectory)
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
			backupFile.delete();
			file.renameTo(backupFile);
		}
		
		else
		{
			new File(backupDirectory,
				backupPrefix + name + backupSuffix
				+ backups + backupSuffix).delete();

			for(int i = backups - 1; i > 0; i--)
			{
				File backup = new File(backupDirectory,
					backupPrefix + name + backupSuffix
					+ i + backupSuffix);

				backup.renameTo(new File(backupDirectory,
					backupPrefix + name + backupSuffix
					+ (i+1) + backupSuffix));
			}

			file.renameTo(new File(backupDirectory,
				backupPrefix + name + backupSuffix
				+ "1" + backupSuffix));
		}
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

	

	

	
	
	public static int getLeadingWhiteSpace(String str)
	{
		int whitespace = 0;
loop:		for(;whitespace < str.length();)
		{
			switch(str.charAt(whitespace))
			{
			case ' ': case '\t':
				whitespace++;
				break;
			default:
				break loop;
			}
		}
		return whitespace;
	} 

	
	
	public static int getTrailingWhiteSpace(String str)
	{
		int whitespace = 0;
loop:		for(int i = str.length() - 1; i >= 0; i--)
		{
			switch(str.charAt(i))
			{
			case ' ': case '\t':
				whitespace++;
				break;
			default:
				break loop;
			}
		}
		return whitespace;
	} 

	
	
	public static int getLeadingWhiteSpaceWidth(String str, int tabSize)
	{
		int whitespace = 0;
loop:		for(int i = 0; i < str.length(); i++)
		{
			switch(str.charAt(i))
			{
			case ' ':
				whitespace++;
				break;
			case '\t':
				whitespace += (tabSize - whitespace % tabSize);
				break;
			default:
				break loop;
			}
		}
		return whitespace;
	} 

	
	
	public static int getVirtualWidth(Segment seg, int tabSize)
	{
		int virtualPosition = 0;

		for (int i = 0; i < seg.count; i++)
		{
			char ch = seg.array[seg.offset + i];

			if (ch == '\t')
			{
				virtualPosition += tabSize
					- (virtualPosition % tabSize);
			}
			else
			{
				++virtualPosition;
			}
		}

		return virtualPosition;
	} 

	
	
	public static int getOffsetOfVirtualColumn(Segment seg, int tabSize,
		int column, int[] totalVirtualWidth)
	{
		int virtualPosition = 0;

		for (int i = 0; i < seg.count; i++)
		{
			char ch = seg.array[seg.offset + i];

			if (ch == '\t')
			{
				int tabWidth = tabSize
					- (virtualPosition % tabSize);
				if(virtualPosition >= column)
					return i;
				else
					virtualPosition += tabWidth;
			}
			else
			{
				if(virtualPosition >= column)
					return i;
				else
					++virtualPosition;
			}
		}

		if(totalVirtualWidth != null)
			totalVirtualWidth[0] = virtualPosition;
		return -1;
	} 

	
	
	public static String createWhiteSpace(int len, int tabSize)
	{
		return createWhiteSpace(len,tabSize,0);
	} 

	
	
	public static String createWhiteSpace(int len, int tabSize, int start)
	{
		StringBuffer buf = new StringBuffer();
		if(tabSize == 0)
		{
			while(len-- > 0)
				buf.append(' ');
		}
		else
		{
			int count = (len + start % tabSize) / tabSize;
			while(count-- > 0)
				buf.append('\t');
			count = (len + start) % tabSize;
			while(count-- > 0)
				buf.append(' ');
		}
		return buf.toString();
	} 

	
	
	public static String globToRE(String glob)
	{
		StringBuffer buf = new StringBuffer();
		boolean backslash = false;
		int insideGroup = 0;
		boolean insideNegativeLookahead = false;

		for(int i = 0; i < glob.length(); i++)
		{
			char c = glob.charAt(i);
			if(backslash)
			{
				buf.append('\\');
				buf.append(c);
				backslash = false;
				continue;
			}

			switch(c)
			{
			case '\\':
				backslash = true;
				break;
			case '?':
				buf.append('.');
				break;
			case '.':
				buf.append("\\.");
				break;
			case '*':
				buf.append(".*");
				break;
			case '{':
				buf.append('(');
				if(i + 1 != glob.length() && glob.charAt(i + 1) == '!')
				{
					buf.append('?');
					insideNegativeLookahead = true;
				}
				else
					insideGroup++;
				break;
			case ',':
				if(insideGroup != 0)
				{
					if(insideNegativeLookahead)
					{
						buf.append(").*");
						insideNegativeLookahead = false;
					}
					buf.append('|');
				}
				else
					buf.append(',');
				break;
			case '}':
				if(insideNegativeLookahead)
				{
					buf.append(").*");
					insideNegativeLookahead = false;
				}
				else if(insideGroup != 0)
				{
					buf.append(')');
					insideGroup--;
				}
				else
					buf.append('}');
				break;
			default:
				buf.append(c);
			}
		}

		return buf.toString();
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

	
	
	public static int compareVersions(String v1, String v2)
	{
		return compareStrings(v1,v2,false);
	} 

	
	
	public static int compareStrings(String str1, String str2, boolean ignoreCase)
	{
		char[] char1 = str1.toCharArray();
		char[] char2 = str2.toCharArray();

		int len = Math.min(char1.length,char2.length);

		for(int i = 0, j = 0; i < len && j < len; i++, j++)
		{
			char ch1 = char1[i];
			char ch2 = char2[j];
			if(Character.isDigit(ch1) && Character.isDigit(ch2)
				&& ch1 != '0' && ch2 != '0')
			{
				int _i = i + 1;
				int _j = j + 1;

				for(; _i < char1.length; _i++)
				{
					if(!Character.isDigit(char1[_i]))
					{
						
						break;
					}
				}

				for(; _j < char2.length; _j++)
				{
					if(!Character.isDigit(char2[_j]))
					{
						
						break;
					}
				}

				int len1 = _i - i;
				int len2 = _j - j;
				if(len1 > len2)
					return 1;
				else if(len1 < len2)
					return -1;
				else
				{
					for(int k = 0; k < len1; k++)
					{
						ch1 = char1[i + k];
						ch2 = char2[j + k];
						if(ch1 != ch2)
							return ch1 - ch2;
					}
				}

				i = _i - 1;
				j = _j - 1;
			}
			else
			{
				if(ignoreCase)
				{
					ch1 = Character.toLowerCase(ch1);
					ch2 = Character.toLowerCase(ch2);
				}

				if(ch1 != ch2)
					return ch1 - ch2;
			}
		}

		return char1.length - char2.length;
	} 

	
	
	public static boolean stringsEqual(String s1, String s2)
	{
		return objectsEqual(s1,s2);
	} 

	
	
	public static boolean objectsEqual(Object o1, Object o2)
	{
		if(o1 == null)
		{
			if(o2 == null)
				return true;
			else
				return false;
		}
		else if(o2 == null)
			return false;
		else
			return o1.equals(o2);
	} 

	
	
	public static String charsToEntities(String str)
	{
		StringBuffer buf = new StringBuffer(str.length());
		for(int i = 0; i < str.length(); i++)
		{
			char ch = str.charAt(i);
			switch(ch)
			{
			case '<':
				buf.append("&lt;");
				break;
			case '>':
				buf.append("&gt;");
				break;
			case '&':
				buf.append("&amp;");
				break;
			default:
				buf.append(ch);
				break;
			}
		}
		return buf.toString();
	} 

	public static final DecimalFormat KB_FORMAT = new DecimalFormat("#.# KB");
	public static final DecimalFormat MB_FORMAT = new DecimalFormat("#.# KB");

	
	public static String formatFileSize(long length)
	{
		if(length < 1024)
			return length + " bytes";
		else if(length < 1024*1024)
			return KB_FORMAT.format((double)length / 1024);
		else
			return MB_FORMAT.format((double)length / 1024 / 1024);
	} 

	

	

	
	
	public static void quicksort(Object[] obj, Comparator compare)
	{
		Arrays.sort(obj,compare);
	} 

	
	
	public static void quicksort(Vector vector, Comparator compare)
	{
		Collections.sort(vector,compare);
	} 

	
	
	public static void quicksort(List list, Comparator compare)
	{
		Collections.sort(list,compare);
	} 

	
	
	public static void quicksort(Object[] obj, Compare compare)
	{
		Arrays.sort(obj,compare);
	} 

	
	
	public static void quicksort(Vector vector, Compare compare)
	{
		Collections.sort(vector,compare);
	} 

	
	
	public interface Compare extends Comparator
	{
		int compare(Object obj1, Object obj2);
	} 

	
	
	public static class StringCompare implements Compare
	{
		public int compare(Object obj1, Object obj2)
		{
			return compareStrings(obj1.toString(),
				obj2.toString(),false);
		}
	} 

	
	
	public static class StringICaseCompare implements Compare
	{
		public int compare(Object obj1, Object obj2)
		{
			return compareStrings(obj1.toString(),
				obj2.toString(),true);
		}
	} 

	
	
	public static class MenuItemCompare implements Compare
	{
		public int compare(Object obj1, Object obj2)
		{
			return compareStrings(((JMenuItem)obj1).getText(),
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

		return "" + major + "." + minor
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

	
	private MiscUtilities() {}

	
}
