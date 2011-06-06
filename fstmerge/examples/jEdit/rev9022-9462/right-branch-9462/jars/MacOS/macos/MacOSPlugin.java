

package macos;


import java.util.Vector;
import javax.swing.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.util.Log;
import macos.menu.*;
import macos.script.*;
import com.apple.cocoa.application.*;
import com.apple.eawt.Application;


public class MacOSPlugin extends EBPlugin
{
	
	static boolean started = false;
	private boolean osok;
	private Delegate delegate;
	
	
	
	public void start()
	{
		if(osok = osok())
		{
			delegate = new Delegate();
			NSApplication app = NSApplication.sharedApplication();
			
			Macros.registerHandler(new AppleScriptHandler());
			Application app2 = new Application();
			app2.addApplicationListener(delegate);
			app2.setEnabledPreferencesMenu(true);
			app2.setEnabledAboutMenu(true);
			
			app.setDelegate(delegate);
			
		}
	} 
	
	
	public void handleMessage(EBMessage message)
	{
		if (osok)
		{
			
			if (message instanceof BufferUpdate)
				delegate.handleFileCodes((BufferUpdate)message);
			else if (message instanceof PropertiesChanged)
			{
				boolean b = jEdit.getBooleanProperty("MacOSPlugin.useSelection",
					jEdit.getBooleanProperty("MacOSPlugin.default.useSelection"));
				if (b)
					jEdit.setColorProperty("view.selectionColor",
						UIManager.getColor("textHighlight"));
			}
			
			
			
			else if (!started && message instanceof ViewUpdate)
				delegate.handleOpenFile((ViewUpdate)message);
		}
	}
	
	
	private boolean osok()
	{
		final String osname = jEdit.getProperty("MacOSPlugin.depend.os.name");
		final String mrjversion = jEdit.getProperty("MacOSPlugin.depend.mrj.version");
		
		if (!System.getProperty("os.name").equals(osname))
		{
			
			Log.log(Log.ERROR,this,jEdit.getProperty("MacOSPlugin.dialog.osname.message"));
			return false;
		}
		if (MiscUtilities.compareStrings(
			System.getProperty("mrj.version"),mrjversion,false) < 0)
		{
			SwingUtilities.invokeLater( new Runnable() { public void run() {
				GUIUtilities.error(null,"MacOSPlugin.dialog.mrjversion",new Object[] {mrjversion});
			}});
			return false;
		}

		return true;
	}
}
