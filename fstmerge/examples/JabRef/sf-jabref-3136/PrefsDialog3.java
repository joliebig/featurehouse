

package net.sf.jabref;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.sf.jabref.groups.GroupsPrefsTab;
import net.sf.jabref.gui.MainTable;
import net.sf.jabref.gui.FileDialogs;

import com.jgoodies.forms.builder.ButtonBarBuilder;


public class PrefsDialog3 extends JDialog {

	JPanel main;

	JabRefFrame frame;

	public PrefsDialog3(JabRefFrame parent) {
		super(parent, Globals.lang("JabRef preferences"), false);
		final JabRefPreferences prefs = JabRefPreferences.getInstance();
		frame = parent;

		final JList chooser;

		JButton importPrefs = new JButton(Globals.lang("Import preferences"));
		JButton exportPrefs = new JButton(Globals.lang("Export preferences"));

		main = new JPanel();
		JPanel upper = new JPanel();
		JPanel lower = new JPanel();

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(upper, BorderLayout.CENTER);
		getContentPane().add(lower, BorderLayout.SOUTH);

		final CardLayout cardLayout = new CardLayout();
		main.setLayout(cardLayout);

		
		
		
		ArrayList<PrefsTab> tabs = new ArrayList<PrefsTab>();
		tabs.add(new GeneralTab(frame, prefs));
        tabs.add(new FileTab(frame, prefs));
        tabs.add(new EntryEditorPrefsTab(frame, prefs));
        tabs.add(new GroupsPrefsTab(prefs));
		tabs.add(new AppearancePrefsTab(prefs));
		tabs.add(new ExternalTab(frame, this, prefs, parent.helpDiag));
		tabs.add(new TablePrefsTab(prefs, parent));
		tabs.add(new TableColumnsTab(prefs, parent));
		tabs.add(new TabLabelPattern(prefs, parent.helpDiag));
		tabs.add(new PreviewPrefsTab(prefs));
		tabs.add(new NameFormatterTab(parent.helpDiag));
		tabs.add(new XmpPrefsTab());
        tabs.add(new AdvancedTab(prefs, parent.helpDiag));
		
		Iterator<PrefsTab> it = tabs.iterator();
		String[] names = new String[tabs.size()];
		int i = 0;
        
        while (it.hasNext()) {
			PrefsTab tab = it.next();
			names[i++] = tab.getTabName(); 
			main.add((Component) tab, tab.getTabName());
        }

		upper.setBorder(BorderFactory.createEtchedBorder());

		chooser = new JList(names);
		chooser.setBorder(BorderFactory.createEtchedBorder());
		
		chooser.setPrototypeCellValue("This should be wide enough");
		chooser.setSelectedIndex(0);
		chooser.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		
		
		chooser.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (e.getValueIsAdjusting())
					return;
				String o = (String) chooser.getSelectedValue();
				cardLayout.show(main, o);
			}
		});

		JPanel one = new JPanel(), two = new JPanel();
		one.setLayout(new BorderLayout());
		two.setLayout(new BorderLayout());
		one.add(chooser, BorderLayout.CENTER);
		one.add(importPrefs, BorderLayout.SOUTH);
		two.add(one, BorderLayout.CENTER);
		two.add(exportPrefs, BorderLayout.SOUTH);
		upper.setLayout(new BorderLayout());
		upper.add(two, BorderLayout.WEST);
		upper.add(main, BorderLayout.CENTER);

		JButton ok = new JButton(Globals.lang("Ok")), cancel = new JButton(Globals.lang("Cancel"));
		ok.addActionListener(new OkAction());
		CancelAction cancelAction = new CancelAction();
		cancel.addActionListener(cancelAction);
		lower.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
		ButtonBarBuilder bb = new ButtonBarBuilder(lower);
		bb.addGlue();
		bb.addGridded(ok);
		bb.addGridded(cancel);
		bb.addGlue();
		
		

		
		ActionMap am = chooser.getActionMap();
		InputMap im = chooser.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
		im.put(frame.prefs().getKey("Close dialog"), "close");
		am.put("close", cancelAction);

		
		exportPrefs.setToolTipText(Globals.lang("Export preferences to file"));
		importPrefs.setToolTipText(Globals.lang("Import preferences from file"));
		exportPrefs.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String filename = FileDialogs.getNewFile(frame, new File(System
					.getProperty("user.home")), ".xml", JFileChooser.SAVE_DIALOG, false);
				if (filename == null)
					return;
				File file = new File(filename);
				if (!file.exists()
					|| (JOptionPane.showConfirmDialog(PrefsDialog3.this, "'" + file.getName()
						+ "' " + Globals.lang("exists. Overwrite file?"), Globals
						.lang("Export preferences"), JOptionPane.OK_CANCEL_OPTION) == JOptionPane.OK_OPTION)) {

					try {
						prefs.exportPreferences(filename);
					} catch (IOException ex) {
						JOptionPane.showMessageDialog(PrefsDialog3.this, Globals
							.lang("Could not export preferences")
							+ ": " + ex.getMessage(), Globals.lang("Export preferences"),
							JOptionPane.ERROR_MESSAGE);
						
					}
				}

			}
		});

		importPrefs.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String filename = FileDialogs.getNewFile(frame, new File(System
					.getProperty("user.home")), ".xml", JFileChooser.OPEN_DIALOG, false);
				if (filename == null)
					return;

				try {
					prefs.importPreferences(filename);
					setValues();
					BibtexEntryType.loadCustomEntryTypes(prefs);
					frame.removeCachedEntryEditors();
                    Globals.prefs.updateEntryEditorTabList();
                } catch (IOException ex) {
					JOptionPane.showMessageDialog(PrefsDialog3.this, Globals
						.lang("Could not import preferences")
						+ ": " + ex.getMessage(), Globals.lang("Import preferences"),
						JOptionPane.ERROR_MESSAGE);
					
				}
			}

		});

		setValues();

		pack(); 

        
    }

	class OkAction extends AbstractAction {
		public OkAction() {
			super("Ok");
		}

		public void actionPerformed(ActionEvent e) {

			AbstractWorker worker = new AbstractWorker() {
				boolean ready = true;

				public void run() {
					
					int count = main.getComponentCount();
					Component[] comps = main.getComponents();
					for (int i = 0; i < count; i++) {
						if (!((PrefsTab) comps[i]).readyToClose()) {
							ready = false;
							return; 
						}
					}
					
					for (int i = 0; i < count; i++) {
						((PrefsTab) comps[i]).storeSettings();
					}
					Globals.prefs.flush();
				}

				public void update() {
					if (!ready)
						return;
					setVisible(false);
					MainTable.updateRenderers();
					frame.setupAllTables();
					frame.groupSelector.revalidateGroups(); 
					
					frame.output(Globals.lang("Preferences recorded."));
				}
			};
			worker.getWorker().run();
			worker.getCallBack().update();

		}
	}

	public void setValues() {
		
		int count = main.getComponentCount();
		Component[] comps = main.getComponents();
		for (int i = 0; i < count; i++) {
			((PrefsTab) comps[i]).setValues();
		}
	}

	class CancelAction extends AbstractAction {
		public CancelAction() {
			super("Cancel");
		}

		public void actionPerformed(ActionEvent e) {
			setVisible(false);
		}
	}

}
