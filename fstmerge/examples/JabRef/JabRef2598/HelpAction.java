
package net.sf.jabref; 

import java.awt.Dimension; 
import java.awt.event.ActionEvent; 
import java.net.URL; 

import javax.swing.ImageIcon; 
import javax.swing.JButton; 
import javax.swing.KeyStroke; 


public  class  HelpAction  extends MnemonicAwareAction {
	

	protected HelpDialog diag;

	

	protected URL helpfile;

	

	protected String helpFile;

	

	public HelpAction(HelpDialog diag, String helpFile) {
		super(GUIGlobals.getImage("help"));
		putValue(NAME, "Help");
		this.diag = diag;
		this.helpFile = helpFile;
	}


	

	public HelpAction(HelpDialog diag, String helpFile, String tooltip) {
		super(GUIGlobals.getImage("help"));
		putValue(NAME, "Help");
		putValue(SHORT_DESCRIPTION, Globals.lang(tooltip));
		this.diag = diag;
		this.helpFile = helpFile;
	}


	

	public HelpAction(HelpDialog diag, String helpFile, String tooltip, URL iconFile) {
		super(new ImageIcon(iconFile));
		putValue(NAME, "Help");
		putValue(SHORT_DESCRIPTION, Globals.lang(tooltip));
		this.diag = diag;
		this.helpFile = helpFile;
	}


	

	public HelpAction(String title, HelpDialog diag, String helpFile, String tooltip) {
		super(GUIGlobals.getImage("help"));
		putValue(NAME, title);
		putValue(SHORT_DESCRIPTION, Globals.lang(tooltip));
		this.diag = diag;
		this.helpFile = helpFile;
	}


	

	public HelpAction(String title, HelpDialog diag, String helpFile, String tooltip, KeyStroke key) {
		super(GUIGlobals.getImage("help"));
		putValue(NAME, title);
		putValue(SHORT_DESCRIPTION, Globals.lang(tooltip));
		putValue(ACCELERATOR_KEY, key);
		this.diag = diag;
		this.helpFile = helpFile;
	}


	

	public HelpAction(String title, HelpDialog diag, String helpFile, String tooltip, URL iconFile) {
		super(new ImageIcon(iconFile));
		putValue(NAME, title);
		putValue(SHORT_DESCRIPTION, Globals.lang(tooltip));
		this.diag = diag;
		this.helpFile = helpFile;
	}


	

	public JButton getIconButton() {
		JButton hlp = new JButton(this);
		hlp.setText(null);
		hlp.setPreferredSize(new Dimension(24, 24));
		return hlp;
	}


	

	public void actionPerformed(ActionEvent e) {
		diag.showPage(helpFile);
	}



}
