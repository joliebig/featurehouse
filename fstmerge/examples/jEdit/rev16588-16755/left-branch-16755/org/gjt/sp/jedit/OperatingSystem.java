

package org.gjt.sp.jedit;

import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.Toolkit;
import javax.swing.UIManager;
import java.io.File;
import java.util.Set;
import java.util.HashSet;

import org.gjt.sp.util.Log;


public class OperatingSystem
{
	
	
	public static Rectangle getScreenBounds()
	{
		int screenX = (int)Toolkit.getDefaultToolkit().getScreenSize().getWidth();
		int screenY = (int)Toolkit.getDefaultToolkit().getScreenSize().getHeight();
		int x, y, w, h;
		
		if (isMacOS())
		{
			x = 0;
			y = 22;
			w = screenX;
			h = screenY - y - 4;
		}
		else if (isWindows())
		{
			x = -4;
			y = -4;
			w = screenX - 2*x;
			h = screenY - 2*y;
		}
		else
		{
			x = 0;
			y = 0;
			w = screenX;
			h = screenY;
		}
		
		return new Rectangle(x,y,w,h);
	} 

	
	
	public static Rectangle getScreenBounds(Rectangle window)
	{
		GraphicsDevice[] gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices();
		Set<GraphicsConfiguration> intersects = new HashSet<GraphicsConfiguration>();

		
		
		
		for (int i=0; i < gd.length; i++)
		{
			GraphicsConfiguration gc = gd[i]
				.getDefaultConfiguration();
			
			if (window.intersects(gc.getBounds()))
			{
				if (!intersects.contains(gc))
					intersects.add(gc);
			}
		}
		
		GraphicsConfiguration choice = null;
		if (!intersects.isEmpty())
		{
			
			for (GraphicsConfiguration gcc : intersects)
			{
				if (choice == null)
					choice = gcc;
				else
				{
					Rectangle int1 = choice.getBounds().intersection(window);
					Rectangle int2 = gcc.getBounds().intersection(window);
					int area1 = int1.width * int1.height;
					int area2 = int2.width * int2.height;
					if (area2 > area1)
						choice = gcc;
				}
			}
		}
		else
			choice = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
		
		
		int screenX = choice.getBounds().x;
		int screenY = choice.getBounds().y;
		int screenW = choice.getBounds().width;
		int screenH = choice.getBounds().height;
		int x, y, w, h;
		
		if (isMacOS())
		{
			x = screenX;
			y = screenY + 22;
			w = screenW;
			h = screenH - y - 4;
		}
		else
		{
			x = screenX;
			y = screenY;
			w = screenW;
			h = screenH;
		}
		
		
		return new Rectangle(x,y,w,h);
	} 

	
	
	public static boolean isDOSDerived()
	{
		return isWindows() || isOS2();
	} 

	
	
	public static boolean isWindows()
	{
		return os == WINDOWS_9x || os == WINDOWS_NT;
	} 

	
	
	public static boolean isWindows9x()
	{
		return os == WINDOWS_9x;
	} 

	
	
	public static boolean isWindowsNT()
	{
		return os == WINDOWS_NT;
	} 

	
	
	public static boolean isOS2()
	{
		return os == OS2;
	} 

	
	
	public static boolean isUnix()
	{
		return os == UNIX || os == MAC_OS_X;
	} 

	
	
	public static boolean isMacOS()
	{
		return os == MAC_OS_X;
	} 

	
	
	public static boolean isX11()
	{
		return os == UNIX;
	} 

	
	
	public static boolean isVMS()
	{
		return os == VMS;
	} 

	
	
	public static boolean isMacOSLF()
	{
		return isMacOS() && UIManager.getLookAndFeel().isNativeLookAndFeel();
	} 

	
	
	public static boolean hasScreenMenuBar()
	{
		if(!isMacOS())
			return false;
		else if(hasScreenMenuBar == -1)
		{
			String result = System.getProperty("apple.laf.useScreenMenuBar");
			if (result == null)
				result = System.getProperty("com.apple.macos.useScreenMenuBar");
			hasScreenMenuBar = "true".equals(result) ? 1 : 0;
		}

		return hasScreenMenuBar == 1;
	} 

	
	
	@Deprecated
	public static boolean hasJava14()
	{
		
		
		
		return java14;
	} 

	
	
	@Deprecated
	public static boolean hasJava15()
	{
		return java15;
	} 

	
	
	public static boolean hasJava16()
	{
		return java16;
	} 

	
	
	public static boolean isCaseInsensitiveFS()
	{
		return isDOSDerived() || isMacOS();
	} 
	
	
	private static final int UNIX = 0x31337;
	private static final int WINDOWS_9x = 0x640;
	private static final int WINDOWS_NT = 0x666;
	private static final int OS2 = 0xDEAD;
	private static final int MAC_OS_X = 0xABC;
	private static final int VMS = 0xDEAD2;
	private static final int UNKNOWN = 0xBAD;

	private static int os;
	private static boolean java14;
	private static boolean java15;
	private static boolean java16;
	private static int hasScreenMenuBar = -1;

	
	static
	{
		if(System.getProperty("mrj.version") != null)
		{
			os = MAC_OS_X;
		}
		else
		{
			String osName = System.getProperty("os.name");
			if(osName.contains("Windows 9")
				|| osName.contains("Windows M"))
			{
				os = WINDOWS_9x;
			}
			else if(osName.contains("Windows"))
			{
				os = WINDOWS_NT;
			}
			else if(osName.contains("OS/2"))
			{
				os = OS2;
			}
			else if(osName.contains("VMS"))
			{
				os = VMS;
			}
			else if(File.separatorChar == '/')
			{
				os = UNIX;
			}
			else
			{
				os = UNKNOWN;
				Log.log(Log.WARNING,OperatingSystem.class,
					"Unknown operating system: " + osName);
			}
		}

		
		
		String javaVersion = System.getProperty("jedit.force.java.version");
		if(javaVersion == null || javaVersion.length() == 0)
			javaVersion = System.getProperty("java.version");
		if(javaVersion == null || javaVersion.length() == 0)
			javaVersion = System.getProperty("java.runtime.version");
		java14 = javaVersion.compareTo("1.4") >= 0;
		java15 = javaVersion.compareTo("1.5") >= 0;
		java16 = javaVersion.compareTo("1.6") >= 0;
	} 

	
}
