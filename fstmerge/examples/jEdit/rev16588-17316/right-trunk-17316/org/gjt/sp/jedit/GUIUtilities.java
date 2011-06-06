

package org.gjt.sp.jedit;


import org.gjt.sp.jedit.browser.VFSFileChooserDialog;
import org.gjt.sp.jedit.gui.DynamicContextMenuService;
import org.gjt.sp.jedit.gui.EnhancedButton;
import org.gjt.sp.jedit.gui.FloatingWindowContainer;
import org.gjt.sp.jedit.gui.SplashScreen;
import org.gjt.sp.jedit.gui.VariableGridLayout;
import org.gjt.sp.jedit.menu.EnhancedCheckBoxMenuItem;
import org.gjt.sp.jedit.menu.EnhancedMenu;
import org.gjt.sp.jedit.menu.EnhancedMenuItem;
import org.gjt.sp.jedit.syntax.SyntaxStyle;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.textarea.TextAreaMouseHandler;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.SyntaxUtilities;


import java.net.URL;
import java.util.*;
import java.util.List;
import java.lang.ref.SoftReference;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;


import java.awt.*;
import java.awt.event.*;



public class GUIUtilities
{
	
	
	@Deprecated
	public static Icon NEW_BUFFER_ICON;

	
	@Deprecated
	public static Icon DIRTY_BUFFER_ICON;

	
	@Deprecated
	public static Icon READ_ONLY_BUFFER_ICON;

	
	@Deprecated
	public static Icon NORMAL_BUFFER_ICON;

	
	@Deprecated
	public static Icon WINDOW_ICON;
	

	

	
	
	public static void setIconPath(String iconPath)
	{
		GUIUtilities.iconPath = iconPath;
		iconCache = null;
	} 

	
	
	public static Icon loadIcon(String iconName)
	{
		if(iconName == null)
			return null;

		
		if(deprecatedIcons != null && deprecatedIcons.containsKey(iconName))
			iconName = deprecatedIcons.get(iconName);

		
		Map<String, Icon> cache = null;
		if(iconCache != null)
		{
			cache = iconCache.get();
		}
		if(cache == null)
		{
			cache = new Hashtable<String, Icon>();
			iconCache = new SoftReference<Map<String, Icon>>(cache);
		}
		Icon icon = cache.get(iconName);
		if(icon != null)
			return icon;

		URL url;

		try
		{
			
			if(MiscUtilities.isURL(iconName))
				url = new URL(iconName);
			else
				url = new URL(iconPath + iconName);
		}
		catch(Exception e)
		{
			try
			{
				url = new URL(defaultIconPath + iconName);
			}
			catch(Exception ex)
			{
				Log.log(Log.ERROR,GUIUtilities.class,
					"Icon not found: " + iconName);
				Log.log(Log.ERROR,GUIUtilities.class,ex);
				return null;
			}
		}

		icon = new ImageIcon(url);

		cache.put(iconName,icon);
		return icon;
	} 

	
	
	public static Image getEditorIcon()
	{
		return ((ImageIcon)loadIcon(jEdit.getProperty("logo.icon.medium"))).getImage();
	} 

	
	
	public static Image getPluginIcon()
	{
		return getEditorIcon();
	} 

	

	

	
	
	public static JMenuBar loadMenuBar(String name)
	{
		return loadMenuBar(jEdit.getActionContext(),name);
	} 

	
	
	public static JMenuBar loadMenuBar(ActionContext context, String name)
	{
		String menus = jEdit.getProperty(name);
		StringTokenizer st = new StringTokenizer(menus);

		JMenuBar mbar = new JMenuBar();

		while(st.hasMoreTokens())
		{
			String menuName = st.nextToken();
			mbar.add(loadMenu(context, menuName));
		}

		return mbar;
	} 

	
	
	public static JMenu loadMenu(String name)
	{
		return loadMenu(jEdit.getActionContext(),name);
	} 

	
	
	public static JMenu loadMenu(ActionContext context, String name)
	{
		return new EnhancedMenu(name,
			jEdit.getProperty(name.concat(".label")),
			context);
	} 

	
	
	public static JPopupMenu loadPopupMenu(String name, JEditTextArea textArea, MouseEvent evt)
	{
		return loadPopupMenu(jEdit.getActionContext(), name, textArea, evt);
	} 

	
	
	public static JPopupMenu loadPopupMenu(String name)
	{
		return loadPopupMenu(jEdit.getActionContext(),name);
	} 

	
	
	public static JPopupMenu loadPopupMenu(ActionContext context, String name)
	{
		return loadPopupMenu(context, name, null, null);
	}

	
	
	public static JPopupMenu loadPopupMenu(ActionContext context, String name, JEditTextArea textArea, MouseEvent evt)
	{
		JPopupMenu menu = new JPopupMenu();

		String menuItems = jEdit.getProperty(name);
		if(menuItems != null)
		{
			StringTokenizer st = new StringTokenizer(menuItems);
			while(st.hasMoreTokens())
			{
				String menuItemName = st.nextToken();
				if(menuItemName.equals("-"))
					menu.addSeparator();
				else
					menu.add(loadMenuItem(context,menuItemName,false));
			}
		}
		
		if (textArea != null)
		{
			List<JMenuItem> list = GUIUtilities.getServiceContextMenuItems(textArea, evt);
			if (!list.isEmpty())
			{
				menu.addSeparator();
			}
			for (JMenuItem mi : list)
			{
				menu.add(mi);
			}
		}

		return menu;
	} 
	
	
	public static List<JMenuItem> getServiceContextMenuItems(JEditTextArea textArea, MouseEvent evt)
	{
		List<JMenuItem> list = new ArrayList<JMenuItem>();
		String serviceClassName =  DynamicContextMenuService.class.getName();
		String[] menuServiceList = ServiceManager.getServiceNames(serviceClassName);
		for (String menuServiceName : menuServiceList)
		{
			if (menuServiceName != null && menuServiceName.trim().length() > 0)
			{
				DynamicContextMenuService dcms = (DynamicContextMenuService)
						ServiceManager.getService(serviceClassName, menuServiceName);
				if (dcms != null)
				{
					JMenuItem[] items = dcms.createMenu(textArea, evt);
					if (items != null)
					{
						list.addAll(Arrays.asList(items));
					}
				}
			}
		}
		return list;
	} 

	
	
	public static JMenuItem loadMenuItem(String name)
	{
		return loadMenuItem(jEdit.getActionContext(),name,true);
	} 

	
	
	public static JMenuItem loadMenuItem(String name, boolean setMnemonic)
	{
		return loadMenuItem(jEdit.getActionContext(),name,setMnemonic);
	} 

	
	
	public static JMenuItem loadMenuItem(ActionContext context, String name,
		boolean setMnemonic)
	{
		if(name.charAt(0) == '%')
			return loadMenu(context,name.substring(1));

		return _loadMenuItem(name, context, setMnemonic);
	} 

	
	public static JMenuItem loadMenuItem(EditAction editAction,
		boolean setMnemonic)
	{
		String name = editAction.getName();
		ActionContext context = jEdit.getActionContext();

		return _loadMenuItem(name, context, setMnemonic);
	} 

	
	
	public static Container loadToolBar(String name)
	{
		return loadToolBar(jEdit.getActionContext(),name);
	} 

	
	
	public static Container loadToolBar(ActionContext context, String name)
	{
		JPanel toolBar = new JPanel(new BorderLayout());
		JToolBar toolB = new JToolBar();
		toolB.setFloatable(false);
		toolB.setMargin(new Insets(0,0,0,0));

		String buttons = jEdit.getProperty(name);
		if(buttons != null)
		{
			StringTokenizer st = new StringTokenizer(buttons);
			while(st.hasMoreTokens())
			{
				String button = st.nextToken();
				if(button.equals("-"))
				{
					toolB.addSeparator(new Dimension(12,12));
				}
				else
				{
					JButton b = loadToolButton(context,button);
					if(b != null)
						toolB.add(b);
				}
			}
		}

		toolBar.add(toolB);
		return toolBar;
	} 

	
	
	public static EnhancedButton loadToolButton(String name)
	{
		return loadToolButton(jEdit.getActionContext(),name);
	} 

	
	
	public static EnhancedButton loadToolButton(ActionContext context,
		String name)
	{
		String label = jEdit.getProperty(name + ".label");

		if(label == null)
			label = name;

		Icon icon;
		String iconName = jEdit.getProperty(name + ".icon");
		if(iconName == null)
			icon = loadIcon(jEdit.getProperty("broken-image.icon"));
		else
		{
			icon = loadIcon(iconName);
			if(icon == null)
				icon = loadIcon(jEdit.getProperty("broken-image.icon"));
		}

		String toolTip = prettifyMenuLabel(label);
		String shortcutLabel = getShortcutLabel(name);
		if(shortcutLabel != null)
		{
			toolTip = toolTip + " (" + shortcutLabel + ')';
		}

		EnhancedButton b = new EnhancedButton(icon,toolTip,name,context);
		b.setPreferredSize(new Dimension(32,32));
		return b;
	} 

	
	
	public static String prettifyMenuLabel(String label)
	{
		int index = label.indexOf('$');
		if(index != -1)
		{
			label = label.substring(0,index)
				.concat(label.substring(index + 1));
		}
		return label;
	} 

	
	
	public static String getShortcutLabel(String action)
	{
		if(action == null)
			return null;
		else
		{
			String shortcut1 = jEdit.getProperty(action + ".shortcut");
			String shortcut2 = jEdit.getProperty(action + ".shortcut2");

			if(shortcut1 == null || shortcut1.length() == 0)
			{
				if(shortcut2 == null || shortcut2.length() == 0)
					return null;
				else
					return shortcut2;
			}
			else
			{
				if(shortcut2 == null || shortcut2.length() == 0)
					return shortcut1;
				else
					return shortcut1 + " or " + shortcut2;
			}
		}
	} 

	

	

	

	
	
	public static void message(Component comp, String name, Object[] args)
	{
		hideSplashScreen();

		JOptionPane.showMessageDialog(comp,
			jEdit.getProperty(name.concat(".message"),args),
			jEdit.getProperty(name.concat(".title"),args),
			JOptionPane.INFORMATION_MESSAGE);
	} 

	
	
	public static void error(Component comp, String name, Object[] args)
	{
		hideSplashScreen();

		JOptionPane.showMessageDialog(comp,
			jEdit.getProperty(name.concat(".message"),args),
			jEdit.getProperty(name.concat(".title"),args),
			JOptionPane.ERROR_MESSAGE);
	} 

	
	
	public static String input(Component comp, String name, Object def)
	{
		return input(comp,name,null,def);
	} 

	
	
	public static String inputProperty(Component comp, String name,
		String def)
	{
		return inputProperty(comp,name,null,def);
	} 

	
	
	public static String input(Component comp, String name,
		Object[] args, Object def)
	{
		hideSplashScreen();

		String retVal = (String)JOptionPane.showInputDialog(comp,
			jEdit.getProperty(name.concat(".message"),args),
			jEdit.getProperty(name.concat(".title")),
			JOptionPane.QUESTION_MESSAGE,null,null,def);
		return retVal;
	} 

	
	
	public static String inputProperty(Component comp, String name,
		Object[] args, String def)
	{
		hideSplashScreen();

		String retVal = (String)JOptionPane.showInputDialog(comp,
			jEdit.getProperty(name.concat(".message"),args),
			jEdit.getProperty(name.concat(".title")),
			JOptionPane.QUESTION_MESSAGE,
			null,null,jEdit.getProperty(def));
		if(retVal != null)
			jEdit.setProperty(def,retVal);
		return retVal;
	} 

	
	
	public static int confirm(Component comp, String name,
		Object[] args, int buttons, int type)
	{
		hideSplashScreen();

		return JOptionPane.showConfirmDialog(comp,
			jEdit.getProperty(name + ".message",args),
			jEdit.getProperty(name + ".title"),buttons,type);
	} 

	
	
	public static int listConfirm(Component comp, String name, String[] args,
		Object[] listModel)
	{
		JList list = new JList(listModel);
		list.setVisibleRowCount(8);

		Object[] message = {
			jEdit.getProperty(name + ".message",args),
			new JScrollPane(list)
		};

		return JOptionPane.showConfirmDialog(comp,
			message,
			jEdit.getProperty(name + ".title"),
			JOptionPane.YES_NO_OPTION,
			JOptionPane.QUESTION_MESSAGE);
	} 

	
	
	public static int listConfirm(Component comp, String name, String[] args,
		Object[] listModel, List selectedItems)
	{
		JList list = new JList(listModel);
		list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		list.setVisibleRowCount(8);
		list.addSelectionInterval(0,listModel.length - 1);

		Object[] message = {
			jEdit.getProperty(name + ".message",args),
			new JScrollPane(list)
		};

		int ret = JOptionPane.showConfirmDialog(comp,
							message,
							jEdit.getProperty(name + ".title"),
							JOptionPane.YES_NO_OPTION,
							JOptionPane.QUESTION_MESSAGE);
		Object[] selectedValues = list.getSelectedValues();
		selectedItems.addAll(Arrays.asList(selectedValues));
		return ret;
	} 

	
	
	public static String[] showVFSFileDialog(View view, String path,
		int type, boolean multipleSelection)
	{
		
		if(view == null)
		{
			Log.log(Log.WARNING,GUIUtilities.class,
			"showVFSFileDialog(): given null view, assuming jEdit.getActiveView()");
			view = jEdit.getActiveView();
		}

		hideSplashScreen();

		VFSFileChooserDialog fileChooser = new VFSFileChooserDialog(
			view,path,type,multipleSelection);
		return fileChooser.getSelectedFiles();
	}

	
	public static String[] showVFSFileDialog(Dialog parent, View view,
		String path, int type, boolean multipleSelection)
	{
		hideSplashScreen();

		VFSFileChooserDialog fileChooser = new VFSFileChooserDialog(
			parent, view, path, type, multipleSelection, true);
		return fileChooser.getSelectedFiles();
	}

	
	public static String[] showVFSFileDialog(Frame parent, View view,
		String path, int type, boolean multipleSelection)
	{
		hideSplashScreen();
		VFSFileChooserDialog fileChooser = new VFSFileChooserDialog(
			parent, view, path, type, multipleSelection, true);
		return fileChooser.getSelectedFiles();
	} 

	

	

	
	
	public static Color parseColor(String name)
	{
		return SyntaxUtilities.parseColor(name, Color.black);
	} 

	
	
	@Deprecated
	public static Color parseColor(String name, Color defaultColor)
	{
		return SyntaxUtilities.parseColor(name, defaultColor);
	} 

	
	
	@Deprecated
	public static String getColorHexString(Color c)
	{
		return SyntaxUtilities.getColorHexString(c);
	} 

	
	
	public static SyntaxStyle parseStyle(String str, String family, int size)
		throws IllegalArgumentException
	{
		return SyntaxUtilities.parseStyle(str,family,size,true);
	} 

	
	
	@Deprecated
	public static SyntaxStyle parseStyle(String str, String family, int size,
		boolean color)
		throws IllegalArgumentException
	{
		return SyntaxUtilities.parseStyle(str,family,size,color);
	} 

	
	
	public static String getStyleString(SyntaxStyle style)
	{
		StringBuilder buf = new StringBuilder();

		if (style.getForegroundColor() != null)
		{
			buf.append("color:").append(SyntaxUtilities.getColorHexString(style.getForegroundColor()));
		}

		if (style.getBackgroundColor() != null)
		{
			buf.append(" bgColor:").append(SyntaxUtilities.getColorHexString(style.getBackgroundColor()));
		}

		Font font = style.getFont();
		if (!font.isPlain())
		{
			buf.append(" style:");
			if (font.isItalic())
				buf.append('i');
			if (font.isBold())
				buf.append('b');
		}

		return buf.toString();
	} 

	
	
	@Deprecated
	public static SyntaxStyle[] loadStyles(String family, int size)
	{
		return SyntaxUtilities.loadStyles(family,size,true);
	}

	
	@Deprecated
	public static SyntaxStyle[] loadStyles(String family, int size, boolean color)
	{
		return SyntaxUtilities.loadStyles(family, size, color);
	} 

	

	

	
	
	public static void loadGeometry(Window win, Container parent, String name )
	{
		Dimension size = win.getSize();
		int width = jEdit.getIntegerProperty(name + ".width", size.width);
		int height = jEdit.getIntegerProperty(name + ".height", size.height);
		int x = jEdit.getIntegerProperty(name + ".x",50);
		int y = jEdit.getIntegerProperty(name + ".y",50);
		if(parent != null)
		{
			Point location = parent.getLocation();
			x = location.x + x;
			y = location.y + y;
		}

		int extState = jEdit.getIntegerProperty(name + ".extendedState", Frame.NORMAL);

		Rectangle desired = new Rectangle(x,y,width,height);
		try
		{
			if(!Debug.DISABLE_MULTIHEAD)
				adjustForScreenBounds(desired);
		}
		catch(Exception e)
		{
			
			Log.log(Log.ERROR,GUIUtilities.class,e);
		}

		if(OperatingSystem.isX11() && Debug.GEOMETRY_WORKAROUND)
			new UnixWorkaround(win,name,desired,extState);
		else
		{
			win.setBounds(desired);
			if(win instanceof Frame)
				((Frame)win).setExtendedState(extState);
		}

	} 

	
	
	public static void loadGeometry(Window win, String name)
	{
		loadGeometry(win, win.getParent(), name);
	} 

	
	
	public static void adjustForScreenBounds(Rectangle desired)
	{
		
		Rectangle osbounds = OperatingSystem.getScreenBounds(desired);

		if (desired.width > osbounds.width)
		{
			desired.width = osbounds.width;
		}
		if (desired.x < osbounds.x)
		{
			desired.x = osbounds.x;
		}
		if (desired.x + desired.width > osbounds.x + osbounds.width)
		{
			desired.x = osbounds.x + osbounds.width - desired.width;
		}
		if (desired.height > osbounds.height)
		{
			desired.height = osbounds.height;
		}
		if (desired.y < osbounds.y)
		{
			desired.y = osbounds.y;
		}
		if (desired.y + desired.height > osbounds.y + osbounds.height)
		{
			desired.y = osbounds.y + osbounds.height - desired.height;
		}
	} 

	
	public static class UnixWorkaround
	{
		Window win;
		String name;
		Rectangle desired;
		Rectangle required;
		long start;
		boolean windowOpened;

		
		public UnixWorkaround(Window win, String name, Rectangle desired,
			int extState)
		{
			this.win = win;
			this.name = name;
			this.desired = desired;

			int adjust_x = jEdit.getIntegerProperty(name + ".dx",0);
			int adjust_y = jEdit.getIntegerProperty(name + ".dy",0);
			int adjust_width = jEdit.getIntegerProperty(name + ".d-width",0);
			int adjust_height = jEdit.getIntegerProperty(name + ".d-height",0);

			required = new Rectangle(
				desired.x - adjust_x,
				desired.y - adjust_y,
				desired.width - adjust_width,
				desired.height - adjust_height);

			Log.log(Log.DEBUG,GUIUtilities.class,"Window " + name
				+ ": desired geometry is " + desired);
			Log.log(Log.DEBUG,GUIUtilities.class,"Window " + name
				+ ": setting geometry to " + required);

			start = System.currentTimeMillis();

			win.setBounds(required);
			if(win instanceof Frame)
				((Frame)win).setExtendedState(extState);

			win.addComponentListener(new ComponentHandler());
			win.addWindowListener(new WindowHandler());
		} 

		
		private class ComponentHandler extends ComponentAdapter
		{
			
			@Override
			public void componentMoved(ComponentEvent evt)
			{
				if(System.currentTimeMillis() - start < 1000L)
				{
					Rectangle r = win.getBounds();
					if(!windowOpened && r.equals(required))
						return;

					if(!r.equals(desired))
					{
						Log.log(Log.DEBUG,GUIUtilities.class,
							"Window resize blocked: " + win.getBounds());
						win.setBounds(desired);
					}
				}

				win.removeComponentListener(this);
			} 

			
			@Override
			public void componentResized(ComponentEvent evt)
			{
				if(System.currentTimeMillis() - start < 1000L)
				{
					Rectangle r = win.getBounds();
					if(!windowOpened && r.equals(required))
						return;

					if(!r.equals(desired))
					{
						Log.log(Log.DEBUG,GUIUtilities.class,
							"Window resize blocked: " + win.getBounds());
						win.setBounds(desired);
					}
				}

				win.removeComponentListener(this);
			} 
		} 

		
		private class WindowHandler extends WindowAdapter
		{
			
			@Override
			public void windowOpened(WindowEvent evt)
			{
				windowOpened = true;

				Rectangle r = win.getBounds();
				Log.log(Log.DEBUG,GUIUtilities.class,"Window "
					+ name + ": bounds after opening: " + r);

				jEdit.setIntegerProperty(name + ".dx",
					r.x - required.x);
				jEdit.setIntegerProperty(name + ".dy",
					r.y - required.y);
				jEdit.setIntegerProperty(name + ".d-width",
					r.width - required.width);
				jEdit.setIntegerProperty(name + ".d-height",
					r.height - required.height);

				win.removeWindowListener(this);
			} 
		} 
	} 

	
	
	public static void saveGeometry(Window win, String name)
	{
		saveGeometry (win, win.getParent(), name);
	} 

	
	
	public static void saveGeometry(Window win, Container parent, String name)
	{
		if(win instanceof Frame)
		{
			jEdit.setIntegerProperty(name + ".extendedState",
				((Frame)win).getExtendedState());
		}

		Rectangle bounds = win.getBounds();
		int x = bounds.x;
		int y = bounds.y;
		if (parent != null)
		{
			Rectangle parentBounds = parent.getBounds();
			x -= parentBounds.x;
			y -= parentBounds.y;
		}
		jEdit.setIntegerProperty(name + ".x",x);
		jEdit.setIntegerProperty(name + ".y",y);
		jEdit.setIntegerProperty(name + ".width", bounds.width);
		jEdit.setIntegerProperty(name + ".height", bounds.height);
	} 

	
	
	@Deprecated
	public static void centerOnScreen(Window win)
	{
		GraphicsDevice gd = GraphicsEnvironment
			.getLocalGraphicsEnvironment()
			.getDefaultScreenDevice();
		Rectangle gcbounds = gd.getDefaultConfiguration().getBounds();
		int x = gcbounds.x + (gcbounds.width - win.getWidth()) / 2;
		int y = gcbounds.y + (gcbounds.height - win.getHeight()) / 2;
		win.setLocation(x,y);
	} 

	

	
	
	public static void hideSplashScreen()
	{
		if(splash != null)
		{
			splash.dispose();
			splash = null;
		}
	} 

	
	
	public static JComponent createMultilineLabel(String str)
	{
		JPanel panel = new JPanel(new VariableGridLayout(
			VariableGridLayout.FIXED_NUM_COLUMNS,1,1,1));
		int lastOffset = 0;
		while(true)
		{
			int index = str.indexOf('\n',lastOffset);
			if(index == -1)
				break;
			else
			{
				panel.add(new JLabel(str.substring(lastOffset,index)));
				lastOffset = index + 1;
			}
		}

		if(lastOffset != str.length())
			panel.add(new JLabel(str.substring(lastOffset)));

		return panel;
	} 

	
	
	public static void requestFocus(final Window win, final Component comp)
	{
		win.addWindowFocusListener(new WindowAdapter()
		{
			@Override
			public void windowGainedFocus(WindowEvent evt)
			{
				SwingUtilities.invokeLater(new Runnable()
				{
						public void run()
						{
							comp.requestFocusInWindow();
						}
				});
				win.removeWindowFocusListener(this);
			}
		});
	} 

	
	
	public static boolean isPopupTrigger(MouseEvent evt)
	{
		return TextAreaMouseHandler.isRightButton(evt.getModifiers());
	} 

	
	
	public static boolean isMiddleButton(int modifiers)
	{
		return TextAreaMouseHandler.isMiddleButton(modifiers);
	} 

	
	
	public static boolean isRightButton(int modifiers)
	{
		return TextAreaMouseHandler.isRightButton(modifiers);
	} 

	
	
	public static Rectangle getScreenBounds()
	{
		Rectangle bounds = GraphicsEnvironment.getLocalGraphicsEnvironment().
			getMaximumWindowBounds();
		GraphicsDevice [] devices = GraphicsEnvironment.
			getLocalGraphicsEnvironment().getScreenDevices();
		if (devices.length > 1)
		{
			for (GraphicsDevice device: devices)
			{
				for (GraphicsConfiguration config: device.getConfigurations())
					bounds = bounds.union(config.getBounds());
			}
		}
		return bounds;
	}

	
	
	public static void showPopupMenu(JPopupMenu popup, Component comp,
		int x, int y)
	{
		showPopupMenu(popup,comp,x,y,true);
	} 

	
	
	public static void showPopupMenu(JPopupMenu popup, Component comp,
		int x, int y, boolean point)
	{
		int offsetX = 0;
		int offsetY = 0;

		int extraOffset = point ? 1 : 0;

		Component win = comp;
		while(!(win instanceof Window || win == null))
		{
			offsetX += win.getX();
			offsetY += win.getY();
			win = win.getParent();
		}

		if(win != null)
		{
			Dimension size = popup.getPreferredSize();

			Rectangle screenSize = getScreenBounds();

			if(x + offsetX + size.width + win.getX() > screenSize.width
				&& x + offsetX + win.getX() >= size.width)
			{
				
				if(point)
					x -= size.width + extraOffset;
				else
					x = win.getWidth() - size.width - offsetX + extraOffset;
			}
			else
			{
				x += extraOffset;
			}

			
			
			
			if(y + offsetY + size.height + win.getY() > screenSize.height
				&& y + offsetY + win.getY() >= size.height)
			{
				if(point)
					y = win.getHeight() - size.height - offsetY + extraOffset;
				else
					y = -size.height - 1;
			}
			else
			{
				y += extraOffset;
			}

			popup.show(comp,x,y);
		}
		else
			popup.show(comp,x + extraOffset,y + extraOffset);

	} 

	
	
	public static boolean isAncestorOf(Component comp1, Component comp2)
	{
		while(comp2 != null)
		{
			if(comp1 == comp2)
				return true;
			else
				comp2 = comp2.getParent();
		}

		return false;
	} 

	
	
	public static JDialog getParentDialog(Component c)
	{
		return (JDialog) SwingUtilities.getAncestorOfClass(JDialog.class, c);
	} 

	
	
	public static Component getComponentParent(Component comp, Class clazz)
	{
		while(true)
		{
			if(comp == null)
				break;

			if(comp instanceof JComponent)
			{
				Component real = (Component)((JComponent)comp)
					.getClientProperty("KORTE_REAL_FRAME");
				if(real != null)
					comp = real;
			}

			if(comp.getClass().equals(clazz))
				return comp;
			else if(comp instanceof JPopupMenu)
				comp = ((JPopupMenu)comp).getInvoker();
			else if(comp instanceof FloatingWindowContainer)
			{
				comp = ((FloatingWindowContainer)comp)
					.getDockableWindowManager();
			}
			else
				comp = comp.getParent();
		}
		return null;
	} 

	
	
	public static void setEnabledRecursively(Container c, boolean enabled)
	{
		for (Component child: c.getComponents())
		{
			if (child instanceof Container)
				setEnabledRecursively((Container)child, enabled);
			else
				child.setEnabled(enabled);
		}
		c.setEnabled(enabled);
	} 

	
	
	public static View getView(Component comp)
	{
		return (View)getComponentParent(comp,View.class);
	} 

	
	
	public static void addSizeSaver(Frame frame, String name)
	{
		addSizeSaver(frame,frame.getParent(),name);
	} 

	
	
	public static void addSizeSaver(Frame frame, Container parent, String name)
	{
		SizeSaver ss = new SizeSaver(frame,parent,name);
		frame.addWindowStateListener(ss);
		frame.addComponentListener(ss);
	} 

	
	
	public static void initContinuousLayout(JSplitPane split)
	{
		boolean continuousLayout = split.isContinuousLayout();
		if (continuousLayout != jEdit.getBooleanProperty("appearance.continuousLayout"))
			split.setContinuousLayout(!continuousLayout);
	} 

	

	
	
	private static void initializeDeprecatedIcons()
	{
		deprecatedIcons.put("File.png",       "16x16/mimetypes/text-x-generic.png");
		deprecatedIcons.put("Folder.png",     "16x16/places/folder.png");
		deprecatedIcons.put("OpenFolder.png", "16x16/status/folder-open.png");
		deprecatedIcons.put("OpenFile.png",   "16x16/actions/edit-select-all.png");
		deprecatedIcons.put("ReloadSmall.png","16x16/actions/view-refresh.png");
		deprecatedIcons.put("DriveSmall.png", "16x16/devices/drive-harddisk.png");
		deprecatedIcons.put("New.png",        "22x22/actions/document-new.png");
		deprecatedIcons.put("NewDir.png",     "22x22/actions/folder-new.png");
		deprecatedIcons.put("Reload.png",     "22x22/actions/view-refresh.png");
		deprecatedIcons.put("Load.png",       "22x22/places/plugins.png");
		deprecatedIcons.put("Save.png",       "22x22/actions/document-save.png");
		deprecatedIcons.put("SaveAs.png",     "22x22/actions/document-save-as.png");
		deprecatedIcons.put("SaveAll.png",    "22x22/actions/document-save-all.png");
		deprecatedIcons.put("Open.png",       "22x22/actions/document-open.png");
		deprecatedIcons.put("Print.png",      "22x22/actions/document-print.png");
		deprecatedIcons.put("Drive.png",      "22x22/devices/drive-harddisk.png");
		deprecatedIcons.put("Clear.png",      "22x22/actions/edit-clear.png");
		deprecatedIcons.put("Run.png",        "22x22/actions/application-run.png");
		deprecatedIcons.put("RunAgain.png",   "22x22/actions/application-run-again.png");
		deprecatedIcons.put("RunToBuffer.png",  "22x22/actions/run-to-buffer.png");
		deprecatedIcons.put("CopyToBuffer.png", "22x22/actions/copy-to-buffer.png");
		deprecatedIcons.put("Plus.png",       "22x22/actions/list-add.png");
		deprecatedIcons.put("Minus.png",      "22x22/actions/list-remove.png");
		deprecatedIcons.put("Find.png",       "22x22/actions/edit-find.png");
		deprecatedIcons.put("FindAgain.png",  "22x22/actions/edit-find-next.png");
		deprecatedIcons.put("FindInDir.png",  "22x22/actions/edit-find-in-folder.png");
		deprecatedIcons.put("Parse.png",      "22x22/actions/document-reload2.png");
		deprecatedIcons.put("Delete.png",     "22x22/actions/edit-delete.png");
		deprecatedIcons.put("Paste.png",      "22x22/actions/edit-paste.png");
		deprecatedIcons.put("Cut.png",        "22x22/actions/edit-cut.png");
		deprecatedIcons.put("Copy.png",       "22x22/actions/edit-copy.png");
		deprecatedIcons.put("Undo.png",       "22x22/actions/edit-undo.png");
		deprecatedIcons.put("Redo.png",       "22x22/actions/edit-redo.png");
		deprecatedIcons.put("CurrentDir.png", "22x22/status/folder-visiting.png");
		deprecatedIcons.put("ParentDir.png",  "22x22/actions/go-parent.png");
		deprecatedIcons.put("PageSetup.png",  "22x22/actions/printer-setup.png");
		deprecatedIcons.put("Plugins.png",    "22x22/apps/system-installer.png");
		deprecatedIcons.put("Floppy.png",     "22x22/devices/media-floppy.png");
		deprecatedIcons.put("Stop.png",       "22x22/actions/process-stop.png");
		deprecatedIcons.put("Cancel.png",     "22x22/actions/process-stop.png");
		deprecatedIcons.put("Home.png",       "22x22/actions/go-home.png");
		deprecatedIcons.put("Help.png",       "22x22/apps/help-browser.png");
		deprecatedIcons.put("Properties.png", "22x22/actions/document-properties.png");
		deprecatedIcons.put("Preferences.png","22x22/categories/preferences-system.png");
		deprecatedIcons.put("ZoomIn.png",     "22x22/actions/zoom-in.png");
		deprecatedIcons.put("ZoomOut.png",    "22x22/actions/zoom-out.png");
		deprecatedIcons.put("BrokenImage.png","22x22/status/image-missing.png");
		deprecatedIcons.put("AdjustWidth.png","22x22/actions/resize-horisontal.png");
		deprecatedIcons.put("ToolbarMenu.gif","ToolbarMenu.gif");

		deprecatedIcons.put("Play.png","22x22/actions/media-playback-start.png");
		deprecatedIcons.put("Pause.png","22x22/actions/media-playback-pause.png");

		deprecatedIcons.put("MultipleResults.png", "22x22/actions/edit-find-multiple.png");
		deprecatedIcons.put("SingleResult.png",    "22x22/actions/edit-find-single.png");

		deprecatedIcons.put("NextFile.png",    "22x22/go-last.png");
		deprecatedIcons.put("PreviousFile.png","22x22/go-first.png");

		deprecatedIcons.put("closebox.gif",   "10x10/actions/close.png");
		deprecatedIcons.put("normal.gif",   "10x10/status/document-unmodified.png");
		deprecatedIcons.put("readonly.gif",   "10x10/emblem/emblem-readonly.png");
		deprecatedIcons.put("dirty.gif",    "10x10/status/document-modified.png");
		deprecatedIcons.put("new.gif",    "10x10/status/document-new.png");

		deprecatedIcons.put("ArrowU.png", "22x22/actions/go-up.png");
		deprecatedIcons.put("ArrowR.png", "22x22/actions/go-next.png");
		deprecatedIcons.put("ArrowD.png", "22x22/actions/go-down.png");
		deprecatedIcons.put("ArrowL.png", "22x22/actions/go-previous.png");
		deprecatedIcons.put("arrow1.png", "16x16/actions/group-expand.png");
		deprecatedIcons.put("arrow2.png", "16x16/actions/group-collapse.png");

		deprecatedIcons.put("NewView.png", "22x22/actions/window-new.png");
		deprecatedIcons.put("UnSplit.png", "22x22/actions/window-unsplit.png");
		deprecatedIcons.put("SplitVertical.png", "22x22/actions/window-split-vertical.png");
		deprecatedIcons.put("SplitHorizontal.png", "22x22/actions/window-split-horizontal.png");

		deprecatedIcons.put("ButtonProperties.png", "22x22/actions/document-properties.png");

	}
	

	
	static void init()
	{
		initializeDeprecatedIcons();

		
		String theme = jEdit.getProperty("icon-theme", "tango");
		Log.log(Log.DEBUG, GUIUtilities.class, "Icon theme set to: "+theme);
		setIconPath("jeditresource:/org/gjt/sp/jedit/icons/themes/" + theme + '/');
		Log.log(Log.DEBUG, GUIUtilities.class, "Loading icon theme from: "+iconPath);

		
		
		NEW_BUFFER_ICON = loadIcon("new.gif");
		DIRTY_BUFFER_ICON = loadIcon("dirty.gif");
		READ_ONLY_BUFFER_ICON = loadIcon("readonly.gif");
		NORMAL_BUFFER_ICON = loadIcon("normal.gif");
		WINDOW_ICON = loadIcon(jEdit.getProperty("logo.icon.medium"));
	} 

	
	static void showSplashScreen()
	{
		splash = new SplashScreen();
	} 

	
	static void advanceSplashProgress()
	{
		if(splash != null)
			splash.advance();
	} 

	
	static void advanceSplashProgress(String label)
	{
		if(splash != null)
			splash.advance(label);
	} 

	

	
	private static SplashScreen splash;
	private static SoftReference<Map<String, Icon>> iconCache;
	private static String iconPath = "jeditresource:/org/gjt/sp/jedit/icons/themes/";
	private static final String defaultIconPath = "jeditresource:/org/gjt/sp/jedit/icons/themes/";
	private static final HashMap<String, String> deprecatedIcons = new HashMap<String, String>();

	
	private static JMenuItem _loadMenuItem(String name, ActionContext context, boolean setMnemonic)
	{

		String label = jEdit.getProperty(name + ".label");
		
		if (label == null)
		{
			label = name;
		}
		char mnemonic;
		int index = label.indexOf('$');
		if (index != -1 && label.length() - index > 1)
		{
			mnemonic = Character.toLowerCase(label.charAt(index + 1));
			label = label.substring(0, index).concat(label.substring(++index));
		}
		else
		{
			mnemonic = '\0';
		}
		JMenuItem mi;
		if (jEdit.getBooleanProperty(name + ".toggle"))
		{
			mi = new EnhancedCheckBoxMenuItem(label, name, context);
		}
		else
		{
			mi = new EnhancedMenuItem(label, name, context);
		}
		if (!OperatingSystem.isMacOS() && setMnemonic && mnemonic != '\0')
		{
			mi.setMnemonic(mnemonic);
		}
		Icon itemIcon = loadIcon(jEdit.getProperty(name + ".icon.small"));
		if(itemIcon != null)
		{
			mi.setIcon(itemIcon);
		}

		return mi;
	} 

	private GUIUtilities() {}
	

	

	
	
	private static class SizeSaver extends ComponentAdapter implements WindowStateListener
	{
		private Frame frame;
		private Container parent;
		private String name;

		
		
		SizeSaver(Frame frame, Container parent, String name)
		{
			if (frame == null || name == null)
			{
				throw new NullPointerException();
			}
			this.frame = frame;
			this.parent = parent;
			this.name = name;
		} 

		
		public void windowStateChanged(WindowEvent wse)
		{
			int extendedState = wse.getNewState();
			jEdit.setIntegerProperty(name + ".extendedState",extendedState);
			Rectangle bounds = frame.getBounds();
			save(extendedState, bounds);
		} 

		
		private void save(int extendedState, Rectangle bounds)
		{
			switch (extendedState)
			{
				case Frame.MAXIMIZED_VERT:
					jEdit.setIntegerProperty(name + ".x",bounds.x);
					jEdit.setIntegerProperty(name + ".width",bounds.width);
					break;

				case Frame.MAXIMIZED_HORIZ:
					jEdit.setIntegerProperty(name + ".y",bounds.y);
					jEdit.setIntegerProperty(name + ".height",bounds.height);
					break;

				case Frame.NORMAL:
					saveGeometry(frame,parent,name );
					break;
			}
		} 

		
		@Override
		public void componentResized(ComponentEvent ce)
		{
			componentMoved(ce);
		} 

		
		@Override
		public void componentMoved(ComponentEvent ce)
		{
			final Rectangle bounds = frame.getBounds();
			final Runnable sizeSaver = new Runnable()
			{
				public void run()
				{
					int extendedState = frame.getExtendedState();
					save(extendedState, bounds);
				}
			};
			new Thread("Sizesavingdelay")
			{
				@Override
				public void run()
				{
					try
					{
						Thread.sleep(500L);
					}
					catch (InterruptedException ie)
					{
					}
					SwingUtilities.invokeLater(sizeSaver);
				}
			}.start();
		} 
	} 

	
}
