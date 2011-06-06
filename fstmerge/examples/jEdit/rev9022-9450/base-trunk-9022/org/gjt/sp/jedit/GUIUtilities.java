

package org.gjt.sp.jedit;



import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;

import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowStateListener;

import java.net.URL;

import java.util.Hashtable;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;

import javax.swing.*;

import org.gjt.sp.jedit.browser.VFSFileChooserDialog;

import org.gjt.sp.jedit.gui.EnhancedButton;
import org.gjt.sp.jedit.gui.FloatingWindowContainer;
import org.gjt.sp.jedit.gui.SplashScreen;
import org.gjt.sp.jedit.gui.VariableGridLayout;

import org.gjt.sp.jedit.menu.EnhancedCheckBoxMenuItem;
import org.gjt.sp.jedit.menu.EnhancedMenu;
import org.gjt.sp.jedit.menu.EnhancedMenuItem;

import org.gjt.sp.jedit.syntax.SyntaxStyle;
import org.gjt.sp.jedit.syntax.Token;

import org.gjt.sp.jedit.textarea.TextAreaMouseHandler;

import org.gjt.sp.util.Log;



public class GUIUtilities
{
	
	
	public static Icon NEW_BUFFER_ICON;

	
	public static Icon DIRTY_BUFFER_ICON;

	
	public static Icon READ_ONLY_BUFFER_ICON;

	
	public static Icon NORMAL_BUFFER_ICON;

	
	public static Icon WINDOW_ICON;
	

	

	
	
	public static void setIconPath(String iconPath)
	{
		GUIUtilities.iconPath = iconPath;
		if(icons != null)
			icons.clear();
	} 

	
	
	public static Icon loadIcon(String iconName)
	{
		if(icons == null)
			icons = new Hashtable<String, Icon>();

		
		Icon icon = icons.get(iconName);
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

		icons.put(iconName,icon);
		return icon;
	} 

	
	
	public static Image getEditorIcon()
	{
		return ((ImageIcon)loadIcon("jedit-icon.gif")).getImage();
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

	
	
	public static JPopupMenu loadPopupMenu(String name)
	{
		return loadPopupMenu(jEdit.getActionContext(),name);
	} 

	
	
	public static JPopupMenu loadPopupMenu(ActionContext context, String name)
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

		return menu;
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

		String label = jEdit.getProperty(name + ".label");
		if(label == null)
			label = name;

		char mnemonic;
		int index = label.indexOf('$');
		if(index != -1 && label.length() - index > 1)
		{
			mnemonic = Character.toLowerCase(label.charAt(index + 1));
			label = label.substring(0,index).concat(label.substring(++index));
		}
		else
			mnemonic = '\0';

		JMenuItem mi;
		if(jEdit.getBooleanProperty(name + ".toggle"))
			mi = new EnhancedCheckBoxMenuItem(label,name,context);
		else
			mi = new EnhancedMenuItem(label,name,context);

		if(!OperatingSystem.isMacOS() && setMnemonic && mnemonic != '\0')
			mi.setMnemonic(mnemonic);

		return mi;
	} 

	
	public static JMenuItem loadMenuItem(EditAction editAction,
		boolean setMnemonic)
	{
		String name = editAction.getName();
		ActionContext context = jEdit.getActionContext();

		
		String label = editAction.getLabel();
		if(label == null)
			label = name;

		char mnemonic;
		int index = label.indexOf('$');
		if(index != -1 && label.length() - index > 1)
		{
			mnemonic = Character.toLowerCase(label.charAt(index + 1));
			label = label.substring(0,index).concat(label.substring(++index));
		}
		else
			mnemonic = '\0';

		JMenuItem mi;
		if(jEdit.getBooleanProperty(name + ".toggle"))
			mi = new EnhancedCheckBoxMenuItem(label,name,context);
		else
			mi = new EnhancedMenuItem(label,name,context);

		if(!OperatingSystem.isMacOS() && setMnemonic && mnemonic != '\0')
			mi.setMnemonic(mnemonic);

		return mi;
	} 

	
	
	public static Box loadToolBar(String name)
	{
		return loadToolBar(jEdit.getActionContext(),name);
	} 

	
	
	public static Box loadToolBar(ActionContext context, String name)
	{
		Box toolBar = new Box(BoxLayout.X_AXIS);

		String buttons = jEdit.getProperty(name);
		if(buttons != null)
		{
			StringTokenizer st = new StringTokenizer(buttons);
			while(st.hasMoreTokens())
			{
				String button = st.nextToken();
				if(button.equals("-"))
					toolBar.add(Box.createHorizontalStrut(12));
				else
				{
					JButton b = loadToolButton(context,button);
					if(b != null)
						toolBar.add(b);
				}
			}
		}

		toolBar.add(Box.createGlue());

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
			icon = loadIcon("BrokenImage.png");
		else
		{
			icon = loadIcon(iconName);
			if(icon == null)
				icon = loadIcon("BrokenImage.png");
		}

		String toolTip = prettifyMenuLabel(label);
		String shortcut1 = jEdit.getProperty(name + ".shortcut");
		String shortcut2 = jEdit.getProperty(name + ".shortcut2");
		if(shortcut1 != null || shortcut2 != null)
		{
			toolTip = toolTip + " ("
				+ (shortcut1 != null
				? shortcut1 : "")
				+ ((shortcut1 != null && shortcut2 != null)
				? " or " : "")
				+ (shortcut2 != null
				? shortcut2
				: "") + ')';
		}

		return new EnhancedButton(icon,toolTip,name,context);
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
		String[] selectedFiles = fileChooser.getSelectedFiles();
		if(selectedFiles == null)
			return null;

		return selectedFiles;
	} 

	

	

	
	
	public static Color parseColor(String name)
	{
		return parseColor(name, Color.black);
	} 

	
	public static Color parseColor(String name, Color defaultColor)
	{
		if(name == null)
			return defaultColor;
		else if(name.charAt(0) == '#')
		{
			try
			{
				return Color.decode(name);
			}
			catch(NumberFormatException nf)
			{
				return defaultColor;
			}
		}
		else if("red".equals(name))
			return Color.red;
		else if("green".equals(name))
			return Color.green;
		else if("blue".equals(name))
			return Color.blue;
		else if("yellow".equals(name))
			return Color.yellow;
		else if("orange".equals(name))
			return Color.orange;
		else if("white".equals(name))
			return Color.white;
		else if("lightGray".equals(name))
			return Color.lightGray;
		else if("gray".equals(name))
			return Color.gray;
		else if("darkGray".equals(name))
			return Color.darkGray;
		else if("black".equals(name))
			return Color.black;
		else if("cyan".equals(name))
			return Color.cyan;
		else if("magenta".equals(name))
			return Color.magenta;
		else if("pink".equals(name))
			return Color.pink;
		else
			return defaultColor;
	} 

	
	
	public static String getColorHexString(Color c)
	{
		String colString = Integer.toHexString(c.getRGB() & 0xffffff);
		return "#000000".substring(0,7 - colString.length()).concat(colString);
	} 

	
	
	public static SyntaxStyle parseStyle(String str, String family, int size)
		throws IllegalArgumentException
	{
		return parseStyle(str,family,size,true);
	} 

	
	
	public static SyntaxStyle parseStyle(String str, String family, int size,
		boolean color)
		throws IllegalArgumentException
	{
		Color fgColor = Color.black;
		Color bgColor = null;
		boolean italic = false;
		boolean bold = false;
		StringTokenizer st = new StringTokenizer(str);
		while(st.hasMoreTokens())
		{
			String s = st.nextToken();
			if(s.startsWith("color:"))
			{
				if(color)
					fgColor = GUIUtilities.parseColor(s.substring(6), Color.black);
			}
			else if(s.startsWith("bgColor:"))
			{
				if(color)
					bgColor = GUIUtilities.parseColor(s.substring(8), null);
			}
			else if(s.startsWith("style:"))
			{
				for(int i = 6; i < s.length(); i++)
				{
					if(s.charAt(i) == 'i')
						italic = true;
					else if(s.charAt(i) == 'b')
						bold = true;
					else
						throw new IllegalArgumentException(
							"Invalid style: " + s);
				}
			}
			else
				throw new IllegalArgumentException(
					"Invalid directive: " + s);
		}
		return new SyntaxStyle(fgColor,bgColor,
			new Font(family,
			(italic ? Font.ITALIC : 0) | (bold ? Font.BOLD : 0),
			size));
	} 

	
	
	public static String getStyleString(SyntaxStyle style)
	{
		StringBuilder buf = new StringBuilder();

		if (style.getForegroundColor() != null)
		{
			buf.append("color:").append(getColorHexString(style.getForegroundColor()));
		}

		if (style.getBackgroundColor() != null)
		{
			buf.append(" bgColor:").append(getColorHexString(style.getBackgroundColor()));
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

	
	
	public static SyntaxStyle[] loadStyles(String family, int size)
	{
		return loadStyles(family,size,true);
	} 

	
	
	public static SyntaxStyle[] loadStyles(String family, int size, boolean color)
	{
		SyntaxStyle[] styles = new SyntaxStyle[Token.ID_COUNT];

		
		for(int i = 1; i < styles.length; i++)
		{
			try
			{
				String styleName = "view.style."
					+ Token.tokenToString((byte)i)
					.toLowerCase(Locale.ENGLISH);
				styles[i] = GUIUtilities.parseStyle(
					jEdit.getProperty(styleName),
					family,size,color);
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,GUIUtilities.class,e);
			}
		}

		return styles;
	} 

	

	

	
	
	public static void loadGeometry(Window win, Container parent, String name ) {
		int x, y, width, height;

		Dimension size = win.getSize();
		width = jEdit.getIntegerProperty(name + ".width", size.width);
		height = jEdit.getIntegerProperty(name + ".height", size.height);
		x = jEdit.getIntegerProperty(name + ".x",50);
		y = jEdit.getIntegerProperty(name + ".y",50);
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

		if(desired.x < osbounds.x || desired.x+desired.width
			> desired.x + osbounds.width)
		{
			if (desired.width > osbounds.width)
				desired.width = osbounds.width;
			desired.x = (osbounds.width - desired.width) / 2;
		}
		if(desired.y < osbounds.y || desired.y+desired.height
			> osbounds.y + osbounds.height)
		{
			if (desired.height >= osbounds.height)
				desired.height = osbounds.height;
			desired.y = (osbounds.height - desired.height) / 2;
		}
	} 

	
	static class UnixWorkaround
	{
		Window win;
		String name;
		Rectangle desired;
		Rectangle required;
		long start;
		boolean windowOpened;

		
		UnixWorkaround(Window win, String name, Rectangle desired,
			int extState)
		{
			this.win = win;
			this.name = name;
			this.desired = desired;

			int adjust_x, adjust_y, adjust_width, adjust_height;
			adjust_x = jEdit.getIntegerProperty(name + ".dx",0);
			adjust_y = jEdit.getIntegerProperty(name + ".dy",0);
			adjust_width = jEdit.getIntegerProperty(name + ".d-width",0);
			adjust_height = jEdit.getIntegerProperty(name + ".d-height",0);

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

		
		class ComponentHandler extends ComponentAdapter
		{
			
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

		
		class WindowHandler extends WindowAdapter
		{
			
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

	
	
	public static void saveGeometry(Window win, String name) {
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
			x = x - parentBounds.x;
			y = y - parentBounds.y;
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
			public void windowGainedFocus(WindowEvent evt)
			{
				SwingUtilities.invokeLater(new Runnable(){
						public void run() {
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

		int extraOffset = (point ? 1 : 0);

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

			Rectangle screenSize = new Rectangle();
            
			GraphicsEnvironment ge = GraphicsEnvironment
				.getLocalGraphicsEnvironment();
			
			GraphicsDevice[] devices = ge.getScreenDevices();
			
			for (int j = 0; j < devices.length; j++) 
			{ 
				GraphicsDevice device = devices[j];
                
				GraphicsConfiguration[] gc =
					device.getConfigurations();
                
				for (int i=0; i < gc.length; i++) 
				{
					screenSize =
						screenSize.union(
							gc[i].getBounds());
				}
			} 

			if(x + offsetX + size.width + win.getX() > screenSize.width
				&& x + offsetX + win.getX() >= size.width)
			{
				
				if(point)
					x -= (size.width + extraOffset);
				else
					x = (win.getWidth() - size.width - offsetX + extraOffset);
			}
			else
			{
				x += extraOffset;
			}

			
			
			
			if(y + offsetY + size.height + win.getY() > screenSize.height
				&& y + offsetY + win.getY() >= size.height)
			{
				if(point)
					y = (win.getHeight() - size.height - offsetY + extraOffset);
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

	

	
	static void init()
	{
		
		
		NEW_BUFFER_ICON = loadIcon("new.gif");
		DIRTY_BUFFER_ICON = loadIcon("dirty.gif");
		READ_ONLY_BUFFER_ICON = loadIcon("readonly.gif");
		NORMAL_BUFFER_ICON = loadIcon("normal.gif");
		WINDOW_ICON = loadIcon("jedit-icon.gif");
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
	private static Map<String, Icon> icons;
	private static String iconPath = "jeditresource:/org/gjt/sp/jedit/icons/";
	private static String defaultIconPath = "jeditresource:/org/gjt/sp/jedit/icons/";

	private GUIUtilities() {}
	

	

	
	
	static class SizeSaver extends ComponentAdapter implements WindowStateListener
	{
		private Frame frame;
		private Container parent;
		private String name;
		
		
		
		SizeSaver(Frame frame, String name)
		{
			this.frame = frame;
			this.parent = frame.getParent();
		} 
		
		
		
		public SizeSaver(Frame frame, Container parent, String name)
		{
			if ((null == frame) || (null == name))
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

		
		public void componentResized(ComponentEvent ce)
		{
			componentMoved(ce);
		} 

		
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
