

package macos;


import com.apple.eawt.*;
import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;
import java.util.Vector;
import java.io.File;
import javax.swing.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.options.GlobalOptions;
import org.gjt.sp.util.Log;


public class Delegate extends ApplicationAdapter
{
	
	private final NSSelector actionSel = new NSSelector("doAction", new Class[] {});
	
	private Buffer lastOpenFile;
	
	
	
	public Delegate()
	{
		if (jEdit.getBooleanProperty("MacOSPlugin.useScreenMenuBar",
			jEdit.getBooleanProperty("MacOSPlugin.default.useScreenMenuBar"))
		)
			System.setProperty("apple.laf.useScreenMenuBar","true");
		else
			System.setProperty("apple.laf.useScreenMenuBar","false");
		
		if (jEdit.getBooleanProperty("MacOSPlugin.liveResize",
			jEdit.getBooleanProperty("MacOSPlugin.default.liveResize"))
		)
			System.setProperty("com.apple.mrj.application.live-resize","true");
		else
			System.setProperty("com.apple.mrj.application.live-resize","false");
	} 
	
	
	
	
	public void handleAbout(ApplicationEvent event)
	{
		new AboutDialog(jEdit.getActiveView());
	} 

	
	public void handleFileCodes(BufferUpdate msg)
	{
		
	} 
	
	
	public void handleOpenFile(ApplicationEvent event)
	{
		File file = new File(event.getFilename());
		Buffer buffer;
		
		View view = PerspectiveManager.loadPerspective(true);
		if(view == null)
			view = jEdit.newView(null);
		
		if ((buffer = jEdit.openFile(view,file.getPath())) != null)
			lastOpenFile = buffer;
		else
			Log.log(Log.ERROR,this,"Error opening file.");
	} 

	
	public void handleOpenFile(ViewUpdate msg)
	{
		if(msg.getWhat() == ViewUpdate.CREATED)
		{
			if(lastOpenFile != null)
				msg.getView().setBuffer(lastOpenFile);
			MacOSPlugin.started = true;
		}
	} 
	
	
	public void handlePreferences(ApplicationEvent event)
	{
		new GlobalOptions(jEdit.getActiveView());
	} 
	
	
	
	public void handleQuit(ApplicationEvent event)
	{
		event.setHandled(false);
		jEdit.exit(jEdit.getActiveView(),true);
	} 

	
	
	
	
	
	public NSMenu applicationDockMenu(NSApplication sender)
	{
		NSMenu dockMenu;
		BufferMenu bufMenu;
		MacrosMenu macMenu;
		RecentMenu recMenu;
		RecentDirMenu dirMenu;
		NSMenuItem showCurrItem;
		NSMenuItem showCurrDirItem;
		NSMenuItem newViewItem;
		
		
		NSMenuItem miBuff = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.buffers.label"),null,"");
		miBuff.setSubmenu(bufMenu = new BufferMenu());
		
		
		NSMenuItem miRec = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.recent.label"),null,"");
		miRec.setSubmenu(recMenu = new RecentMenu());
		
		
		NSMenuItem miDir = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.recentDir.label"),null,"");
		miDir.setSubmenu(dirMenu = new RecentDirMenu());
		
		
		NSMenuItem miMac = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.macros.label"),null,"");
		miMac.setSubmenu(macMenu = new MacrosMenu());
		
		dockMenu = new NSMenu();
		newViewItem = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.newView"),actionSel,"");
		newViewItem.setTarget(new NewViewAction());
		dockMenu.addItem(newViewItem);
		dockMenu.addItem(new NSMenuItem().separatorItem());
		showCurrItem = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.showCurrent"),actionSel,"");
		dockMenu.addItem(showCurrItem);
		showCurrDirItem = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.showCurrentDir"),actionSel,"");
		dockMenu.addItem(showCurrDirItem);
		dockMenu.addItem(new NSMenuItem().separatorItem());
		dockMenu.addItem(miBuff);
		dockMenu.addItem(miRec);
		dockMenu.addItem(miDir);
		dockMenu.addItem(new NSMenuItem().separatorItem());
		dockMenu.addItem(miMac);
		if (jEdit.getViewCount() == 0)
			miMac.setEnabled(false);
		
		bufMenu.updateMenu();
		recMenu.updateMenu();
		dirMenu.updateMenu();
		macMenu.updateMenu();
		
		View view = jEdit.getActiveView();
		if (view != null)
		{
			File buff = new File(view.getBuffer().getPath());
			if (buff.exists())
			{
				showCurrItem.setTarget(new ShowFileAction(buff.getPath()));
				showCurrDirItem.setTarget(new ShowFileAction(buff.getParent()));
			}
		}
		else
		{
			showCurrItem.setEnabled(false);
			showCurrDirItem.setEnabled(false);
		}
		
		return dockMenu;
	} 
	
	
	public boolean applicationShouldHandleReopen(NSApplication theApplication, boolean flag)
	{
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				if (jEdit.getViewCount() == 0)
					jEdit.newView(null);
			}
		});
		
		return false;
	} 
	
	
	public boolean applicationShouldTerminate(NSApplication sender)
	{
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				jEdit.exit(jEdit.getActiveView(),true);
			}
		});
		return false;
	}
	
	
	
	
	
	
	
	public String openFile(NSPasteboard pboard, String userData)
	{
		return null;
	} 
	
	
	public String openSelection(NSPasteboard pboard, String userData)
	{
		jEdit.newFile(jEdit.getActiveView()).insert(0,userData);
		return null;
	} 
	
	
	
	
	
	
	class BufferMenu extends NSMenu
	{
		public BufferMenu()
		{
			super();
		}
		
		public void updateMenu()
		{
			NSMenuItem item;
			for (int i=0; i<numberOfItems(); i++)
				removeItemAtIndex(0);
			
			Buffer[] buffs = jEdit.getBuffers();
			for (int i=0; i < buffs.length; i++)
			{
				if (!buffs[i].isUntitled())
				{
					item = new NSMenuItem(buffs[i].getName(),actionSel,"");
					item.setTarget(new ShowFileAction(buffs[i].getPath()));
					
					
					if (!new File(buffs[i].getPath()).exists())
						item.setEnabled(false);
					addItem(item);
				}
			}
			
			if (numberOfItems() == 0)
			{
				item = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.buffers.none"),null,"");
				item.setEnabled(false);
				addItem(item);
			}
		}
	} 
	
	
	class MacrosMenu extends NSMenu
	{
		public MacrosMenu()
		{
			super();
		}
		
		public void updateMenu()
		{
			Vector macroVector = Macros.getMacroHierarchy();
			NSMenuItem item;
			File file;
			int max = macroVector.size();
			
			int length = numberOfItems();
			for (int i=0; i<length; i++)
				removeItemAtIndex(0);
			
			if (max == 0)
			{
				item = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.macros.none"),null,"");
				item.setEnabled(false);
				addItem(item);
				return;
			}
			
			createMenu(this,macroVector);
		}
		
		public void createMenu(NSMenu menu, Vector vector)
		{
			for(int i=0; i < vector.size(); i++)
			{
				Object obj = vector.elementAt(i);
				if(obj instanceof Macros.Macro)
				{
					Macros.Macro macro = (Macros.Macro)obj;
					NSMenuItem item = new NSMenuItem(macro.getLabel(),actionSel,"");
					item.setTarget(new MacroAction(macro));
					menu.addItem(item);
				}
				else if(obj instanceof Vector)
				{
					Vector subvector = (Vector)obj;
					String name = (String)subvector.elementAt(0);
					NSMenu submenu = new NSMenu();
					createMenu(submenu,subvector);
					if(submenu.numberOfItems() > 0)
					{
						NSMenuItem submenuitem = new NSMenuItem(name,null,"");
						submenuitem.setSubmenu(submenu);
						menu.addItem(submenuitem);
					}
				}
			}
		}
	} 
	
	
	class RecentMenu extends NSMenu
	{
		public RecentMenu()
		{
			super();
		}
		
		public void updateMenu()
		{
			Vector recent = BufferHistory.getBufferHistory();
			NSMenuItem item;
			File file;
			int max = recent.size();
			int min = max - 20;
			
			int length = numberOfItems();
			for (int i=0; i<length; i++)
				removeItemAtIndex(0);
			
			if (max == 0)
			{
				item = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.recent.none"),null,"");
				item.setEnabled(false);
				addItem(item);
				return;
			}
			
			if (min < 0)
				min = 0;
			
			for (int i=max-1; i >= min ; i--)
			{
				file = new File(((BufferHistory.Entry)recent.get(i)).path);
				item = new NSMenuItem(file.getName(),actionSel,"");
				item.setTarget(new ShowFileAction(file.getPath()));
				
				if (!file.exists())
					item.setEnabled(false);
				addItem(item);
			}
		}
	} 
	
	
	class RecentDirMenu extends NSMenu
	{
		public RecentDirMenu()
		{
			super();
		}
		
		public void updateMenu()
		{
			HistoryModel model = HistoryModel.getModel("vfs.browser.path");
			NSMenuItem item;
			File file;
			int max = model.getSize();
			
			int length = numberOfItems();
			for (int i=0; i<length; i++)
				removeItemAtIndex(0);
			
			if (max == 0)
			{
				item = new NSMenuItem(jEdit.getProperty("MacOSPlugin.menu.recentDir.none"),null,"");
				item.setEnabled(false);
				addItem(item);
				return;
			}
			
			for (int i=0; i < max ; i++)
			{
				file = new File(model.getItem(i));
				item = new NSMenuItem(file.getName(),actionSel,"");
				item.setTarget(new ShowFileAction(file.getPath()));
				
				if (!file.exists())
					item.setEnabled(false);
				addItem(item);
			}
		}
	} 
	
	
	class MacroAction
	{
		private Macros.Macro macro;
		
		public MacroAction(Macros.Macro macro)
		{
			this.macro = macro;
		}
		
		public void doAction()
		{
			macro.invoke(jEdit.getActiveView());
		}
	} 
	
	
	class NewViewAction
	{
		public void doAction()
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					View view = PerspectiveManager.loadPerspective(true);
					if(view == null)
						jEdit.newView(null);
					else
						jEdit.newView(jEdit.getActiveView());
				}
			});
		}
	} 
	
	
	class ShowFileAction
	{
		private String path;
		
		public ShowFileAction(String path)
		{
			this.path = path;
		}
		
		public void doAction()
		{
			MacOSActions.showInFinder(path);
		}
	} 
	
	
}
