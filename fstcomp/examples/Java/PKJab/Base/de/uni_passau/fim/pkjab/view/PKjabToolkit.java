package de.uni_passau.fim.pkjab.view;

import java.awt.Color;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.UIDefaults;
import javax.swing.UIManager;

import de.uni_passau.fim.pkjab.util.UserState;

final class PKjabToolkit {

    public static final String ICON_DIR = "/icons/";
    
    public static final String PKJAB_DIR = System.getProperty("user.home") 
				+ "/PKjab";
	
	private static AbstractTheme theme = new DefaultTheme();
	
	private static final HashMap statusIcons = new HashMap(6);

	private PKjabToolkit() {
		// prevent instantiation
	}
	
	static ImageIcon getImageIcon(final String name) {
    	final URL u = PKjabToolkit.class.getResource(name);
    	if (u == null) {
    		return null;
    	}
		return new ImageIcon(u);
    }
	
	static void setupUI() {
		
		UIDefaults defaults = UIManager.getDefaults();
		
		ArrayList components = new ArrayList();
		components.add("Button");
		components.add("Panel");
		components.add("Label");
		components.add("TextField");
		components.add("TextArea");
		components.add("ComboBox");
		components.add("SplitPane");
		components.add("Separator");
		components.add("ScrollPane"); 
		components.add("Viewport");
		components.add("OptionPane");
		components.add("PopupMenu");
		components.add("Menu");
		components.add("MenuItem");
		components.add("PasswordField");
		components.add("ScrollBar");
		components.add("CheckBox");
		components.add("Slider");
		components.add("TabbedPane");
		components.add("InternalFrame");
		components.add("RadioButton");
		components.add("ColorChooser");
		
		Iterator it = components.iterator();

		String s;
		for ( ; it.hasNext(); ) {
		    s = (String) it.next();
			defaults.put(s + ".background", theme.getBackgroundColor());
			defaults.put(s + ".foreground", theme.getForegroundColor());
			defaults.put(s + ".font", theme.getFont());
		}
		
		defaults.put("TextArea.caretForeground", theme.getForegroundColor());
		defaults.put("TextField.caretForeground", theme.getForegroundColor());
		defaults.put("OptionPane.messageForeground", theme.getForegroundColor());
		defaults.put("ScrollBar.foreground", theme.getBackgroundColor());
		defaults.put("ScrollBar.highlight", theme.getBackgroundColor());
		defaults.put("Separator.highlight", theme.getBackgroundColor());
		
		defaults.put("Button.focus", theme.getForegroundColor());
		defaults.put("TitledBorder.titleColor", theme.getForegroundColor());
		defaults.put("TitledBorder.font", theme.getFont());
	}
	
	static AbstractTheme getTheme() {
		return theme;
	}
	
	static HashMap getStatusIcons() {
		return statusIcons;
	}
	
	static void setTheme(AbstractTheme nTheme) {
		theme = nTheme;
	}
}
