

package macosx;


import javax.swing.*;
import java.util.regex.Pattern;
import java.io.File;

import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.options.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;


public class MacOSXPlugin extends EBPlugin
{
	
	private boolean osok = false;
	private static final Pattern ctrlPat = Pattern.compile("\\bctrl\\b");
	
	
	public void MacOSXPlugin()
	{
	}
	
	
	public void start()
	{
		if(osok())
		{
			try {
				
				
				OSXAdapter.setQuitHandler(this, getClass().getDeclaredMethod("handleQuit", (Class[])null));
				OSXAdapter.setAboutHandler(this, getClass().getDeclaredMethod("handleAbout", (Class[])null));
				OSXAdapter.setPreferencesHandler(this, getClass().getDeclaredMethod("handlePreferences", (Class[])null));
				OSXAdapter.setFileHandler(this, getClass().getDeclaredMethod("handleOpenFile", new Class[] { String.class }));
				OSXAdapter.setReOpenApplicationHandler(this, getClass().getDeclaredMethod("handleReOpenApplication", (Class[])null));
				
				String lf = jEdit.getProperty("lookAndFeel");
				if(lf != null && lf.length() != 0)
				{
					
					
					if (lf.equalsIgnoreCase("apple.laf.aqualookandfeel") == false)
					{
						Log.log(Log.DEBUG, this, "Fixing keybindingds on current LNF");
						UIDefaults uid = UIManager.getLookAndFeelDefaults();
						fixMacKeyBindings(uid);
					}
				}
			}
			catch (Exception e)
			{
				System.err.println("Error while loading the OSXAdapter:");
				e.printStackTrace();
			}
		}
	} 
	
	public void stop()
	{
		
		
	}
	
	
	
	public boolean handleQuit()
	{
		jEdit.exit(jEdit.getActiveView(), true);
		return false;
	}
	
	public void handlePreferences()
	{
		new GlobalOptions(jEdit.getActiveView());
	}
	
	
	
	public void handleAbout()
	{
		new AboutDialog(jEdit.getActiveView());
	}
	
	public void handleOpenFile(String filepath)
	{
		File file = new File(filepath);
		if(file.exists())
		{
			View view = jEdit.getActiveView();
			if(view == null)
				view = PerspectiveManager.loadPerspective(false);
			
			if(file.isDirectory())
			{
				
				
				return;
			}
			if (jEdit.openFile(view, file.getPath()) == null)
				Log.log(Log.ERROR, this, "Unable to open file: " + filepath);
		}
		else
		{
			Log.log(Log.ERROR, this, "Cannot open non-existing file: " + filepath);
		}
		
	}
	
	public void handleReOpenApplication()
	{
		if(jEdit.getActiveView() != null)
		{
			jEdit.getActiveView().requestFocus();
		}
		else
		{
			PerspectiveManager.loadPerspective(true);
		}
	}
	
	public void handleMessage(EBMessage message)
	{
		if(message instanceof BufferUpdate)
		{
			BufferUpdate msg = (BufferUpdate)message;
			refreshViewModification(jEdit.getActiveView());
		}
		else if(message instanceof ViewUpdate)
		{
			ViewUpdate msg = (ViewUpdate)message;
			refreshViewModification(msg.getView());
		}
		else if(message instanceof EditPaneUpdate)
		{
			EditPaneUpdate msg = (EditPaneUpdate)message;
			View view = msg.getEditPane().getView();
			if(view != null)
			{
				if(view.getBuffer() != null)
				{
					String path = view.getBuffer().getPath();
					view.getRootPane().putClientProperty("Window.documentFile", new File(path));
				}
			}
		}
	}
	
	public void refreshViewModification(View view)
	{
		boolean modifiedView = false;
		if(view != null)
		{
			EditPane[] editPanes = view.getEditPanes();
			for(int i = 0; i < editPanes.length; i++)
			{
				Buffer[] buffers = editPanes[i].getBufferSet().getAllBuffers();
				for(int j = 0; j < buffers.length; j++)
				{
					Buffer buf = buffers[j];
					if(buf.isDirty())
					{
						Log.log(Log.DEBUG, this, "Dirty buffer found");
						modifiedView = true;
						break;
					}
				}
			}
			Log.log(Log.DEBUG, this, "Dirty buffer found? " + modifiedView + " for " + editPanes.length + " buffers");
			if(modifiedView)
			{
				view.getRootPane().putClientProperty("Window.documentModified", Boolean.TRUE);
				view.getRootPane().putClientProperty("windowModified", Boolean.TRUE); 
			}
			else
			{
				view.getRootPane().putClientProperty("Window.documentModified", Boolean.FALSE);
				view.getRootPane().putClientProperty("windowModified", Boolean.FALSE); 
			}
		} 
	}
	
	public static void fixMacKeyBindings(UIDefaults uiDefaults)
	{
		Object[] keys = uiDefaults.keySet().toArray(); 
		
		for (Object key : keys)
		{
			Object  value = uiDefaults.get(key);
			
			if (value instanceof InputMap)
			{
				InputMap map = (InputMap) value;
				KeyStroke[] keyStrokes = map.keys();
				
				if (keyStrokes != null)
				{
					for (KeyStroke keyStroke : keyStrokes)
					{
						String  keyString = keyStroke.toString();
						
						if (keyString.indexOf("ctrl ") >= 0)
						{
							Object  action = map.get(keyStroke);
							
							keyString = ctrlPat.matcher(keyString).replaceAll("meta");
							map.remove(keyStroke);
							keyStroke = KeyStroke.getKeyStroke(keyString);
							map.put(keyStroke, action);
						}
					}
				}
			}
		}
	}

	
	private boolean osok()
	{
		final String mrjversion = jEdit.getProperty("MacOSXPlugin.depend.mrj.version");
		
		if (!OperatingSystem.isMacOS())
		{
			
			Log.log(Log.ERROR,this,jEdit.getProperty("MacOSXPlugin.dialog.osname.message"));
			return false;
		}
		
		if (StandardUtilities.compareStrings(System.getProperty("mrj.version"),mrjversion,false) < 0)
		{
			SwingUtilities.invokeLater( new Runnable() { public void run() {
				GUIUtilities.error(null,"MacOSXPlugin.dialog.mrjversion",new Object[] {mrjversion});
			}});
			return false;
		}

		return true;
	}
}
