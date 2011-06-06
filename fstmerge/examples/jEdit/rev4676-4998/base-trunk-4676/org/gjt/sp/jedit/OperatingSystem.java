

package org.gjt.sp.jedit;

import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.Toolkit;
import javax.swing.UIManager;
import java.io.File;
import java.util.Enumeration;
import java.util.Vector;
import org.gjt.sp.util.Log;


public class OperatingSystem
{
	
	
	public static final Rectangle getScreenBounds()
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
	
	
	
	public static final Rectangle getScreenBounds(Rectangle window)
	{
		GraphicsDevice[] gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices();
		Vector intersects = new Vector();
		
		
		
		
		for (int i=0; i < gd.length; i++)
		{
			GraphicsConfiguration[] gc = gd[i].getConfigurations();
L2:			for (int j=0; j < gc.length; j++)
			{
				
				if (window.intersects(gc[j].getBounds()))
				{
					for (Enumeration e = intersects.elements(); e.hasMoreElements();)
					{
						GraphicsConfiguration gcc = (GraphicsConfiguration)e.nextElement();
						if (gcc.getBounds().equals(gc[j].getBounds()))
							continue L2;
					}
					intersects.add(gc[j]);
				}
			}
		}
		
		GraphicsConfiguration choice = null;
		if (intersects.size() > 0)
		{
			
			for (Enumeration e = intersects.elements(); e.hasMoreElements();)
			{
				GraphicsConfiguration gcc = (GraphicsConfiguration)e.nextElement();
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
		
		
		int screenX = (int)choice.getBounds().x;
		int screenY = (int)choice.getBounds().y;
		int screenW = (int)choice.getBounds().width;
		int screenH = (int)choice.getBounds().height;
		int x, y, w, h;
		
		if (isMacOS())
		{
			x = screenX;
			y = screenY + 22;
			w = screenW;
			h = screenH - y - 4;
		}
		else if (isWindows())
		{
			x = screenX - 4;
			y = screenY - 4;
			w = screenW - 2*x;
			h = screenH - 2*y;
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
	
	
	
	public static final boolean isDOSDerived()
	{
		return isWindows() || isOS2();
	} 

	
	
	public static final boolean isWindows()
	{
		return os == WINDOWS_9x || os == WINDOWS_NT;
	} 

	
	
	public static final boolean isWindows9x()
	{
		return os == WINDOWS_9x;
	} 

	
	
	public static final boolean isWindowsNT()
	{
		return os == WINDOWS_NT;
	} 

	
	
	public static final boolean isOS2()
	{
		return os == OS2;
	} 

	
	
	public static final boolean isUnix()
	{
		return os == UNIX || os == MAC_OS_X;
	} 

	
	
	public static final boolean isMacOS()
	{
		return os == MAC_OS_X;
	} 

	
	
	public static final boolean isVMS()
	{
		return os == VMS;
	} 

	
	
	public static final boolean isMacOSLF()
	{
		return (isMacOS() && UIManager.getLookAndFeel().isNativeLookAndFeel());
	} 

	
	
	public static final boolean hasScreenMenuBar()
	{
		if(!isMacOS())
			return false;
		else if(hasScreenMenuBar == -1)
		{
			String result = System.getProperty("apple.laf.useScreenMenuBar");
			if (result == null)
				result = System.getProperty("com.apple.macos.useScreenMenuBar");
			hasScreenMenuBar = (result.equals("true")) ? 1 : 0;
		}

		return (hasScreenMenuBar == 1);
	} 

	
	
	public static final boolean hasJava14()
	{
		return java14;
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
			if(osName.indexOf("Windows 9") != -1
				|| osName.indexOf("Windows M") != -1)
			{
				os = WINDOWS_9x;
			}
			else if(osName.indexOf("Windows") != -1)
			{
				os = WINDOWS_NT;
			}
			else if(osName.indexOf("OS/2") != -1)
			{
				os = OS2;
			}
			else if(osName.indexOf("VMS") != -1)
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

		if(System.getProperty("java.version").compareTo("1.4") >= 0
			&& System.getProperty("jedit.nojava14") == null)
			java14 = true;
	} 

	
}
