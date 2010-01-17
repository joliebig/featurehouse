

package net.sf.jabref;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;



public class PrefsDialog2 extends JDialog {

    private JabRefPreferences _prefs;
    JPanel upper = new JPanel(),
        lower = new JPanel();
    GridBagLayout gbl = new GridBagLayout();
    GridBagConstraints con = new GridBagConstraints();
    JTabbedPane tabbed = new JTabbedPane();
    JabRefFrame frame;

    public PrefsDialog2(JabRefFrame parent, JabRefPreferences prefs) {
        super(parent, Globals.lang("JabRef preferences"), false);
        _prefs = prefs;
        frame = parent;
        getContentPane().setLayout(gbl);
        con.weighty = 1;
        con.weightx = 1;
        con.fill = GridBagConstraints.BOTH;
        con.gridwidth = GridBagConstraints.REMAINDER;
        con.insets = new Insets(5, 5, 0, 5);
        gbl.setConstraints(tabbed, con);
        getContentPane().add(tabbed);
        con.weighty = 0;
        con.gridheight = GridBagConstraints.REMAINDER;
        gbl.setConstraints(lower, con);
        
        
        getContentPane().add(lower);

        
        
        
        
        tabbed.addTab(Globals.lang("General"), new GeneralTab(frame, _prefs));
        tabbed.addTab(Globals.lang("Appearance"), new TablePrefsTab(_prefs, parent));
        
        tabbed.addTab(Globals.lang("Key pattern"), new TabLabelPattern(_prefs, parent.helpDiag));
        tabbed.addTab(Globals.lang("Entry preview"), new PreviewPrefsTab(_prefs));
        
	if (!Globals.ON_MAC)
	    tabbed.addTab(Globals.lang("Advanced"), new AdvancedTab(_prefs, parent.helpDiag));
        JButton
            ok = new JButton(Globals.lang("Ok")),
            cancel = new JButton(Globals.lang("Cancel"));
        ok.addActionListener(new OkAction());
        CancelAction cancelAction = new CancelAction();
        cancel.addActionListener(cancelAction);
        lower.add(ok);
        lower.add(cancel);

        
        ActionMap am = tabbed.getActionMap();
        InputMap im = tabbed.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(frame.prefs().getKey("Close dialog"), "close");
        am.put("close", cancelAction);

        pack(); 
    }

    class OkAction extends AbstractAction {
        public OkAction() {
            super("Ok");
        }
        public void actionPerformed(ActionEvent e) {

	    AbstractWorker worker = new AbstractWorker() {
		    public void run() {
			
			for (int i = 0; i < tabbed.getTabCount(); i++) {
			    if (!((PrefsTab)tabbed.getComponentAt(i)).readyToClose())
				return; 
			}			
			
			for (int i = 0; i < tabbed.getTabCount(); i++) {
			    ( (PrefsTab) tabbed.getComponentAt(i)).storeSettings();
			}

			
		    }
		    public void update() {
			dispose();
			frame.setupAllTables();
			frame.output(Globals.lang("Preferences recorded."));
		    }
		};
            worker.getWorker().run();
            worker.getCallBack().update();
            
        }
    }

    class CancelAction extends AbstractAction {
        public CancelAction() {
            super("Cancel");

        }
        public void actionPerformed(ActionEvent e) {
	    dispose();
            
            
        }
    }

}
