package net.sf.jabref;

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

import net.sf.jabref.gui.ColorSetupPanel;

import com.jgoodies.forms.builder.DefaultFormBuilder;
import com.jgoodies.forms.layout.FormLayout;

class AppearancePrefsTab extends JPanel implements PrefsTab {

    JabRefPreferences _prefs;
    private JCheckBox colorCodes, antialias, overrideFonts;
    private GridBagLayout gbl = new GridBagLayout();
    private JButton fontButton = new JButton(Globals.lang("Set table font"));
    private ColorSetupPanel colorPanel = new ColorSetupPanel();
    private Font font = GUIGlobals.CURRENTFONT;
    private int oldMenuFontSize;
    private boolean oldOverrideFontSize;
    private JTextField fontSize;

    
    public AppearancePrefsTab(JabRefPreferences prefs) {
        _prefs = prefs;
         setLayout(new BorderLayout());

        
        fontSize = new JTextField(5);


        colorCodes = new JCheckBox(Globals.lang
                   ("Color codes for required and optional fields"));
        antialias = new JCheckBox(Globals.lang
                  ("Use antialiasing font"));
        overrideFonts = new JCheckBox(Globals.lang("Override default font settings"));

        
        
        FormLayout layout = new FormLayout
                ("1dlu, 8dlu, left:pref, 4dlu, fill:pref, 4dlu, fill:60dlu, 4dlu, fill:pref",
                        "");
        DefaultFormBuilder builder = new DefaultFormBuilder(layout);
        builder.setLeadingColumnOffset(2);
        JLabel lab;
        builder.appendSeparator(Globals.lang("General"));
        JPanel p1 = new JPanel();
        lab = new JLabel(Globals.lang("Menu and label font size") + ":");
        p1.add(lab);
        p1.add(fontSize);
        builder.append(p1);
        builder.nextLine();
        builder.append(overrideFonts);
        builder.nextLine();
        builder.appendSeparator(Globals.lang("Table appearance"));
        builder.append(antialias);
        builder.nextLine();
        builder.append(colorCodes);
        builder.nextLine();
        builder.append(fontButton);
        builder.nextLine();
        builder.append(colorPanel);
        

    JPanel upper = new JPanel(),
        sort = new JPanel(),
        namesp = new JPanel(),
            iconCol = new JPanel();
    upper.setLayout(gbl);
    sort.setLayout(gbl);
        namesp.setLayout(gbl);
        iconCol.setLayout(gbl);


    overrideFonts.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
            fontSize.setEnabled(overrideFonts.isSelected());
        }
    });

    fontButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
            
            Font f=new FontSelectorDialog
                (null, GUIGlobals.CURRENTFONT).getSelectedFont();
            if(f==null)
                return;
            else
                font = f;
        }
        });
    

    JPanel pan = builder.getPanel();
    pan.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
    add(pan, BorderLayout.CENTER);
    }

    public void setValues() {
        colorCodes.setSelected(_prefs.getBoolean("tableColorCodesOn"));
        antialias.setSelected(_prefs.getBoolean("antialias"));
        fontSize.setText("" + _prefs.getInt("menuFontSize"));
        oldMenuFontSize = _prefs.getInt("menuFontSize");
        overrideFonts.setSelected(_prefs.getBoolean("overrideDefaultFonts"));
        oldOverrideFontSize = overrideFonts.isSelected();
        fontSize.setEnabled(overrideFonts.isSelected());
        
        
        colorPanel.setValues();
    }

    
    public void storeSettings() {

        _prefs.putBoolean("tableColorCodesOn", colorCodes.isSelected());
        _prefs.putBoolean("antialias", antialias.isSelected());
        _prefs.put("fontFamily", font.getFamily());
        _prefs.putInt("fontStyle", font.getStyle());
        _prefs.putInt("fontSize", font.getSize());
        _prefs.putBoolean("overrideDefaultFonts", overrideFonts.isSelected());
        GUIGlobals.CURRENTFONT = font;
        colorPanel.storeSettings();
        try {
            int size = Integer.parseInt(fontSize.getText());
            if ((overrideFonts.isSelected() != oldOverrideFontSize) ||
                    (size != oldMenuFontSize)) {
                _prefs.putInt("menuFontSize", size);
                JOptionPane.showMessageDialog(null, Globals.lang("You have changed the menu and label font size. "
                        + "You must restart JabRef for this to come into effect."), Globals.lang("Changed font settings"),
                        JOptionPane.WARNING_MESSAGE);
            }

        } catch (NumberFormatException ex) {
            ex.printStackTrace();
        }
    }

    public boolean readyToClose() {
        try {
            
            Integer.parseInt(fontSize.getText());
        } catch (NumberFormatException ex) {
            JOptionPane.showMessageDialog
                    (null, Globals.lang("You must enter an integer value in the text field for") + " '" +
                            Globals.lang("Menu and label font size") + "'", Globals.lang("Changed font settings"),
                            JOptionPane.ERROR_MESSAGE);
            return false;
        }
        return true;

    }

	public String getTabName() {
	    return Globals.lang("Appearance");
	}  
}
