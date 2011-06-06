

package macos;


import com.apple.eawt.*;
import com.apple.eio.*;
import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;
import java.util.*;
import java.io.File;
import javax.swing.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.browser.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.options.GlobalOptions;
import org.gjt.sp.util.Log;


public class Delegate extends ApplicationAdapter
{
	
	private final NSSelector actionSel = new NSSelector("doAction", new Class[] {});
	
	private List filenames = new LinkedList();
	
	
	
	public Delegate()
	{
		if (jEdit.getBooleanProperty("MacOSPlugin.useScreenMenuBar",
			jEdit.getBooleanProperty("MacOSPlugin.default.useScreenMenuBar"))
		)
			System.setProperty("apple.laf.useScreenMenuBar","true");
		else
			System.setProperty("apple.laf.useScreenMenuBar","false");
	} 
	
	
	
	
	public void handleAbout(ApplicationEvent event)
	{
		event.setHandled(true);
		new AboutDialog(jEdit.getActiveView());
	} 

	
	public void handleFileCodes(BufferUpdate msg)
	{
		Buffer buffer = msg.getBuffer();
		
		
		if (!buffer.isDirty() && msg.getWhat() == BufferUpdate.DIRTY_CHANGED)
		{
			try {
				FileManager.setFileTypeAndCreator(buffer.getPath(),
					buffer.getIntegerProperty("MacOSPlugin.type",
						jEdit.getIntegerProperty("MacOSPlugin.default.type",0)),
					buffer.getIntegerProperty("MacOSPlugin.creator",
						jEdit.getIntegerProperty("MacOSPlugin.default.creator",0)));
			} catch (Exception e) {
				
			}
		}
		
		else if (msg.getWhat() == BufferUpdate.CREATED)
		{			
			if ("true".equals(
				jEdit.getProperty("MacOSPlugin.preserveCodes")))
			{
				try {
					int type = FileManager.getFileType(buffer.getPath());
					int creator = FileManager.getFileCreator(buffer.getPath());
					
					if (type != 0)
						buffer.setIntegerProperty("MacOSPlugin.type",type);
					if (creator != 0)
						buffer.setIntegerProperty("MacOSPlugin.creator",creator);
				} catch (Exception e) {
					
				}
			}
		}
	} 
	
	
	public void handleOpenFile(ApplicationEvent event)
	{
		filenames.add(event.getFilename());
		event.setHandled(true);
	} 

	
	public void handleOpenFile(ViewUpdate msg)
	{
		if(msg.getWhat() == ViewUpdate.CREATED)
		{
			Iterator i = filenames.iterator();
			while (i.hasNext())
				jEdit.openFile(msg.getView(),(String)i.next());
			MacOSPlugin.started = true;
			NSApplication app = NSApplication.sharedApplication();
			app.setServicesProvider(new Delegate());
		}
	} 
	
	
	public void handlePreferences(ApplicationEvent event)
	{
		event.setHandled(true);
		new GlobalOptions(jEdit.getActiveView());
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
	
	
	public void applicationOpenFiles(NSApplication sender, NSArray filenames)
	{
		int count = filenames.count();
		for (int i=0; i<count; i++)
		{
			File file = new File((String)filenames.objectAtIndex(i));
			Buffer buffer;
			
			View view = jEdit.getActiveView();
			if(view == null)
				view = PerspectiveManager.loadPerspective(true);
			
			if (file.isDirectory())
			{
				VFSBrowser.browseDirectory(jEdit.getActiveView(),file.getPath());
				return;
			}
			
			if (jEdit.openFile(view,file.getPath()) == null)
				Log.log(Log.ERROR,this,"Error opening file.");
		}
	} 
	
	
	public boolean applicationShouldHandleReopen(NSApplication theApplication, boolean flag)
	{
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				if (jEdit.getViewCount() == 0)
					new NewViewAction().doAction();
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
		if (jEdit.getViewCount() == 0)
			return null;
		
		NSData data = pboard.dataForType("NSFilenamesPboardType");
		String[] error = new String[1];
		int[] format = new int[1];
		NSArray filenames = (NSArray)NSPropertyListSerialization.propertyListFromData(data,
			NSPropertyListSerialization.PropertyListImmutable,
			format,
			error);
		int count = filenames.count();
		for (int i=0; i<count; i++)
		{
			File file = new File((String)filenames.objectAtIndex(i));
			if (file.isDirectory())
				VFSBrowser.browseDirectory(jEdit.getActiveView(),file.getPath());
			else
				jEdit.openFile(jEdit.getActiveView(),file.getPath());
		}
		
		return null;
	} 
	
	
	public String insertSelection(NSPasteboard pboard, String userData)
	{
		String string = pboard.stringForType("NSStringPboardType");
		if (jEdit.getViewCount() > 0)
		{
			View view = jEdit.getActiveView();
			view.getBuffer().insert(view.getTextArea().getCaretPosition(),string);
		}
		return null;
	} 
	
	
	public String openSelection(NSPasteboard pboard, String userData)
	{
		String string = pboard.stringForType("NSStringPboardType");
		if (jEdit.getViewCount() == 0)
			new NewViewAction().doAction();
		jEdit.newFile(jEdit.getActiveView()).insert(0,pboard.stringForType("NSStringPboardType"));
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
			List recent = BufferHistory.getHistory();
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
					if (jEdit.getViewCount() == 0)
						PerspectiveManager.loadPerspective(true);
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
