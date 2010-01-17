package net.sf.jabref.imports;

import java.net.URL;

import javax.swing.JPanel;

import net.sf.jabref.JabRefFrame;
import net.sf.jabref.gui.ImportInspectionDialog;


public interface EntryFetcher extends ImportInspectionDialog.CallBack {

	
	public void processQuery(String query, ImportInspectionDialog dialog,
			JabRefFrame frame);

	
	public String getTitle();

	
	public String getKeyName();

	
	public URL getIcon();

	
	public String getHelpPage();

	
	public JPanel getOptionsPanel();
}
