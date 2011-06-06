

package org.gjt.sp.jedit;


import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.util.*;
import org.gjt.sp.jedit.browser.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.syntax.SyntaxStyle;
import org.gjt.sp.jedit.syntax.Token;
import org.gjt.sp.util.Log;



public class GUIUtilities
{
	
	public static final Icon NEW_BUFFER_ICON = loadIcon("new.gif");
	public static final Icon DIRTY_BUFFER_ICON = loadIcon("dirty.gif");
	public static final Icon READ_ONLY_BUFFER_ICON = loadIcon("readonly.gif");
	public static final Icon NORMAL_BUFFER_ICON = loadIcon("normal.gif");
	public static final Icon WINDOW_ICON = loadIcon("jedit-icon.gif");
	

	

	
	
	public static Icon loadIcon(String iconName)
	{
		if(icons == null)
			icons = new Hashtable();

		
		Icon icon = (Icon)icons.get(iconName);
		if(icon != null)
			return icon;

		
		if(iconName.startsWith("file:"))
		{
			icon = new ImageIcon(iconName.substring(5));
		}
		else
		{
			URL url = GUIUtilities.class.getResource(
				"/org/gjt/sp/jedit/icons/" + iconName);
			if(url == null)
			{
				Log.log(Log.ERROR,GUIUtilities.class,
					"Icon not found: " + iconName);
				return null;
			}

			icon = new ImageIcon(url);
		}

		icons.put(iconName,icon);
		return icon;
	} 

	
	
	public static Image getEditorIcon()
	{
		return ((ImageIcon)WINDOW_ICON).getImage();
	} 

	
	
	public static Image getPluginIcon()
	{
		return ((ImageIcon)WINDOW_ICON).getImage();
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
			mbar.add(loadMenu(context,st.nextToken()));
		}

		return mbar;
	} 

	
	
	public static JMenu loadMenu(String name)
	{
		return loadMenu(jEdit.getActionContext(),name);
	} 

	
	
	public static JMenu loadMenu(ActionContext context, String name)
	{
		String customMenu = jEdit.getProperty(name + ".custom-menu");
		if(customMenu != null)
		{
			Object eval = BeanShell.eval(null,BeanShell.getNameSpace(),customMenu);
			if(eval instanceof JMenu)
				return (JMenu)eval;
			else
				return null;
		}
		else
		{
			return new EnhancedMenu(name,
				jEdit.getProperty(name.concat(".label")),
				context);
		}
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
		if(name.startsWith("%"))
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

	
	
	public static JToolBar loadToolBar(String name)
	{
		return loadToolBar(jEdit.getActionContext(),name);
	} 

	
	
	public static JToolBar loadToolBar(ActionContext context, String name)
	{
		JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);

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
				: "") + ")";
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

	
	
	public static String[] showVFSFileDialog(View view, String path,
		int type, boolean multipleSelection)
	{
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
		else if(name.startsWith("#"))
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
		StringBuffer buf = new StringBuffer();

		if(style.getForegroundColor() != null)
		{
			buf.append("color:" + getColorHexString(style.getForegroundColor()));
		}

		if(style.getBackgroundColor() != null) 
		{
			buf.append(" bgColor:" + getColorHexString(style.getBackgroundColor()));
		}
		if(!style.getFont().isPlain())
		{
			buf.append(" style:" + (style.getFont().isItalic() ? "i" : "")
				+ (style.getFont().isBold() ? "b" : ""));
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
					.toLowerCase();
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

	

	

	
	
	public static void loadGeometry(Window win, String name)
	{
		int x, y, width, height;

		Dimension size = win.getSize();
		GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
		Rectangle gcbounds = gd.getDefaultConfiguration().getBounds();

		x = gcbounds.x;
		y = gcbounds.y;

		width = jEdit.getIntegerProperty(name + ".width",size.width);
		height = jEdit.getIntegerProperty(name + ".height",size.height);

		Component parent = win.getParent();
		if(parent == null)
		{
			x += (gcbounds.width - width) / 2;
			y += (gcbounds.height - height) / 2;
		}
		else
		{
			Rectangle bounds = parent.getBounds();
			x += bounds.x + (bounds.width - width) / 2;
			y += bounds.y + (bounds.height - height) / 2;
		}

		x = jEdit.getIntegerProperty(name + ".x",x);
		y = jEdit.getIntegerProperty(name + ".y",y);

		int extState = jEdit.getIntegerProperty(name + ".extendedState", 0);
		setWindowBounds(win,x,y,width,height,extState);
	} 

	
	
	public static void setWindowBounds(Window win, int x, int y,
		int width, int height, int extState)
	{
		
		Rectangle osbounds = OperatingSystem.getScreenBounds(new Rectangle(x,y,width,height));

		if(x < osbounds.x || x+width > osbounds.width)
		{
			if (width > osbounds.width)
				width = osbounds.width;
			x = (osbounds.width - width) / 2;
		}
		if(y < osbounds.y || y+height > osbounds.height)
		{
			if (height >= osbounds.height)
				height = osbounds.height;
			y = (osbounds.height - height) / 2;
		}

		Rectangle desired = new Rectangle(x,y,width,height);
		win.setBounds(desired);

		if(win instanceof Frame)
			setExtendedState((Frame)win,extState);
	} 

	
	
	public static void saveGeometry(Window win, String name)
	{
		if(win instanceof Frame)
		{
			jEdit.setIntegerProperty(name + ".extendedState",
				getExtendedState((Frame)win));
		}

		Rectangle bounds = win.getBounds();
		jEdit.setIntegerProperty(name + ".x",bounds.x);
		jEdit.setIntegerProperty(name + ".y",bounds.y);
		jEdit.setIntegerProperty(name + ".width",bounds.width);
		jEdit.setIntegerProperty(name + ".height",bounds.height);
	} 

	
	
	public static int getExtendedState(Frame frame)
	{
		if(OperatingSystem.hasJava14())
		{
			try
			{
				java.lang.reflect.Method meth =
					Frame.class.getMethod("getExtendedState",
					new Class[0]);

				Integer extState = (Integer)meth.invoke(frame,
					new Object[0]);

				return extState.intValue();
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,GUIUtilities.class,e);
			}
		}

		return 0;
	} 

	
	
	public static void setExtendedState(Frame frame, int extState)
	{
		if(OperatingSystem.hasJava14())
		{
			try
			{
				java.lang.reflect.Method meth =
					Frame.class.getMethod("setExtendedState",
					new Class[] {int.class});

				meth.invoke(frame, new Object[] {
					new Integer(extState)});
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,GUIUtilities.class,e);
			}
		}
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
		for(;;)
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
		win.addWindowListener(new WindowAdapter()
		{
			public void windowActivated(WindowEvent evt)
			{
				comp.requestFocus();
				win.removeWindowListener(this);
			}
		});
	} 

	
	
	public static boolean isPopupTrigger(MouseEvent evt)
	{
		return isRightButton(evt.getModifiers());
	} 

	
	
	public static boolean isMiddleButton(int modifiers)
	{
		if (OperatingSystem.isMacOS())
		{
			if((modifiers & MouseEvent.BUTTON1_MASK) != 0)
				return ((modifiers & MouseEvent.META_MASK) != 0);
			if(!OperatingSystem.hasJava14())
				return ((modifiers & MouseEvent.BUTTON3_MASK) != 0);
			else
				return ((modifiers & MouseEvent.BUTTON2_MASK) != 0);
		}
		else
			return ((modifiers & MouseEvent.BUTTON2_MASK) != 0);
	} 

	
	
	public static boolean isRightButton(int modifiers)
	{
		if (OperatingSystem.isMacOS())
		{
			if((modifiers & MouseEvent.BUTTON1_MASK) != 0)
				return ((modifiers & MouseEvent.CTRL_MASK) != 0);
			if(!OperatingSystem.hasJava14())
				return ((modifiers & MouseEvent.BUTTON2_MASK) != 0);
			else
				return ((modifiers & MouseEvent.BUTTON3_MASK) != 0);
		}
		else
			return ((modifiers & MouseEvent.BUTTON3_MASK) != 0);
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

			Rectangle screenSize = win.getGraphicsConfiguration()
				.getBounds();

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
					y = comp.getY() - size.height - 1;
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
		Component p = c.getParent();
		while (p != null && !(p instanceof JDialog))
			p = p.getParent();

		return (p instanceof JDialog) ? (JDialog) p : null;
	} 

	
	
	public static Component getComponentParent(Component comp, Class clazz)
	{
		for(;;)
		{
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
			else if(comp != null)
				comp = comp.getParent();
			else
				break;
		}
		return null;
	} 

	
	
	public static View getView(Component comp)
	{
		return (View)getComponentParent(comp,View.class);
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

	

	
	private static SplashScreen splash;
	private static Hashtable icons;

	private GUIUtilities() {}
	
}
