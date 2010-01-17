package net.sf.jabref;

import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.builder.DefaultFormBuilder;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;
import java.awt.*;
import java.util.Iterator;
import java.text.SimpleDateFormat;

public class EntryEditorPrefsTab extends JPanel implements PrefsTab {

    private JCheckBox autoOpenForm, showSource,
        defSource, editSource, disableOnMultiple, autoComplete;
    private JTextField autoCompFields;
    JabRefPreferences _prefs;
    JabRefFrame _frame;

    public EntryEditorPrefsTab(JabRefFrame frame, JabRefPreferences prefs) {
        _prefs = prefs;
        _frame = frame;
        setLayout(new BorderLayout());


        autoOpenForm = new JCheckBox(Globals.lang("Open editor when a new entry is created"));
        defSource = new JCheckBox(Globals.lang("Show BibTeX source by default"));
        showSource = new JCheckBox(Globals.lang("Show BibTeX source panel"));
        editSource = new JCheckBox(Globals.lang("Enable source editing"));
        disableOnMultiple = new JCheckBox(Globals.lang("Disable entry editor when multiple entries are selected"));
        autoComplete = new JCheckBox(Globals.lang("Enable word/name autocompletion"));
        autoCompFields = new JTextField(40);
        JPanel general = new JPanel();
        Insets marg = new Insets(0,12,3,0);
        editSource.setMargin(marg);
        defSource.setMargin(marg);
        
        showSource.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent event) {
                defSource.setEnabled(showSource.isSelected());
                editSource.setEnabled(showSource.isSelected());
            }
        }
        );
        
        
        autoComplete.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent event) {
                autoCompFields.setEnabled(autoComplete.isSelected());
            }
        }
        );

        FormLayout layout = new FormLayout
                ("8dlu, left:pref, 8dlu, fill:150dlu, 4dlu, fill:pref", 
                        "pref, 6dlu, pref, 6dlu, pref, 6dlu, pref, 6dlu, pref, 6dlu, "
                                    +"pref, 6dlu, pref, 6dlu, pref, 6dlu, pref");
        DefaultFormBuilder builder = new DefaultFormBuilder(layout);
        CellConstraints cc = new CellConstraints();
        builder.addSeparator(Globals.lang("Editor options"), cc.xyw(1,1, 5));
        builder.add(autoOpenForm, cc.xy(2, 3));
        builder.add(disableOnMultiple, cc.xy(2, 5));
        builder.add(showSource, cc.xy(2, 7));
        builder.add(defSource, cc.xy(2, 9));
        builder.add(autoComplete, cc.xy(2, 11));
        JLabel label = new JLabel(Globals.lang("Use autocompletion for the following fields")+":");
        DefaultFormBuilder builder3 = new DefaultFormBuilder
                (new FormLayout("left:pref, 4dlu, fill:150dlu",""));
        builder3.append(label);
        builder3.append(autoCompFields);
        builder.add(builder3.getPanel(), cc.xyw(2, 13, 3));

        JPanel pan = builder.getPanel();
        pan.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        add(pan, BorderLayout.CENTER);

    }

    public void setValues() {
        autoOpenForm.setSelected(_prefs.getBoolean("autoOpenForm"));
        defSource.setSelected(_prefs.getBoolean("defaultShowSource"));
        showSource.setSelected(_prefs.getBoolean("showSource"));
        editSource.setSelected(_prefs.getBoolean("enableSourceEditing"));
        disableOnMultiple.setSelected(_prefs.getBoolean("disableOnMultipleSelection"));
        autoComplete.setSelected(_prefs.getBoolean("autoComplete"));
        autoCompFields.setText(_prefs.get("autoCompleteFields"));
        
        defSource.setEnabled(showSource.isSelected());
        editSource.setEnabled(showSource.isSelected());
        
        autoCompFields.setEnabled(autoComplete.isSelected());
        
    }

    public void storeSettings() {
        _prefs.putBoolean("autoOpenForm", autoOpenForm.isSelected());
        _prefs.putBoolean("defaultShowSource", defSource.isSelected());
        _prefs.putBoolean("enableSourceEditing", editSource.isSelected());
        _prefs.putBoolean("disableOnMultipleSelection", disableOnMultiple.isSelected());
        
        boolean oldAutoComplete = _prefs.getBoolean("autoComplete");
        boolean oldShowSource = _prefs.getBoolean("showSource");
        String oldAutoCompFields = _prefs.get("autoCompleteFields");
        _prefs.putBoolean("autoComplete", autoComplete.isSelected());
        _prefs.put("autoCompleteFields", autoCompFields.getText());
        _prefs.putBoolean("showSource", showSource.isSelected());
        
        
        if ((oldShowSource != showSource.isSelected()) || (oldAutoComplete != autoComplete.isSelected())
                || (!oldAutoCompFields.equals(autoCompFields.getText()))) {
            for (int j=0; j<_frame.getTabbedPane().getTabCount(); j++) {
	            BasePanel bp = (BasePanel)_frame.getTabbedPane().getComponentAt(j);
	            bp.entryEditors.clear();
            }
        }

    }

    public boolean readyToClose() {
        return true;
    }

	public String getTabName() {
		return Globals.lang("Entry editor");
	}
}
