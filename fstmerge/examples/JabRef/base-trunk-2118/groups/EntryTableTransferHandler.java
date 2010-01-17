

package net.sf.jabref.groups;

import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.event.InputEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.TransferHandler;

import net.sf.jabref.BasePanel;
import net.sf.jabref.Globals;
import net.sf.jabref.JabRefFrame;
import net.sf.jabref.external.DroppedFileHandler;
import net.sf.jabref.external.ExternalFileType;
import net.sf.jabref.gui.MainTable;
import net.sf.jabref.imports.ImportMenuItem;
import net.sf.jabref.imports.OpenDatabaseAction;
import net.sf.jabref.imports.ParserResult;
import net.sf.jabref.net.URLDownload;

public class EntryTableTransferHandler extends TransferHandler {

	protected final MainTable entryTable;

	protected JabRefFrame frame;

	private BasePanel panel;

	protected DataFlavor urlFlavor;

	protected DataFlavor stringFlavor;

	protected static boolean DROP_ALLOWED = true;

	
	public EntryTableTransferHandler(MainTable entryTable, JabRefFrame frame, BasePanel panel) {
		this.entryTable = entryTable;
		this.frame = frame;
		this.panel = panel;
		stringFlavor = DataFlavor.stringFlavor;
		try {
			urlFlavor = new DataFlavor("application/x-java-url; class=java.net.URL");
		} catch (ClassNotFoundException e) {
			Globals.logger("Unable to configure drag and drop for main table");
			e.printStackTrace();
		}
	}

	
	public int getSourceActions(JComponent c) {
		return DnDConstants.ACTION_LINK;
	}

	
	public Transferable createTransferable(JComponent c) {
		
		return new TransferableEntrySelection(entryTable.getSelectedEntries());
	}

	
	public boolean importData(JComponent comp, Transferable t) {

		
		
		int dropRow = -1;
		if (comp instanceof JTable) {
			dropRow = ((JTable) comp).getSelectedRow();
		}

		try {

			
			if (t.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
				
				
				List l = (List) t.getTransferData(DataFlavor.javaFileListFlavor);
				return handleDraggedFiles(l, dropRow);
			}

			if (t.isDataFlavorSupported(urlFlavor)) {
				URL dropLink = (URL) t.getTransferData(urlFlavor);
				return handleDropTransfer(dropLink, dropRow);
			}

			if (t.isDataFlavorSupported(stringFlavor)) {
				
				
				String dropStr = (String) t.getTransferData(stringFlavor);
				return handleDropTransfer(dropStr, dropRow);
			}

		} catch (IOException ioe) {
			System.err.println("failed to read dropped data: " + ioe.toString());
		} catch (UnsupportedFlavorException ufe) {
			System.err.println("drop type error: " + ufe.toString());
		}

		
		System.err.println("can't transfer input: ");
		DataFlavor inflavs[] = t.getTransferDataFlavors();
		for (int i = 0; i < inflavs.length; i++) {
			System.out.println("  " + inflavs[i].toString());
		}

		return false;
	}

	
	public boolean canImport(JComponent comp, DataFlavor[] transferFlavors) {
		if (!DROP_ALLOWED)
			return false;

		
		for (int i = 0; i < transferFlavors.length; i++) {
			DataFlavor inflav = transferFlavors[i];
			if (inflav.match(urlFlavor) || inflav.match(stringFlavor)
				|| inflav.match(DataFlavor.javaFileListFlavor))
				return true;
		}

		
		
		return false;
	}

	public void exportAsDrag(JComponent comp, InputEvent e, int action) {
		
		super.exportAsDrag(comp, e, DnDConstants.ACTION_LINK);
	}

	protected void exportDone(JComponent source, Transferable data, int action) {
		
		super.exportDone(source, data, action);
	}

	public void exportToClipboard(JComponent comp, Clipboard clip, int action) {
		
		super.exportToClipboard(comp, clip, action);
	}

	

	protected boolean handleDropTransfer(String dropStr, final int dropRow) throws IOException {
		if (dropStr.startsWith("file:")) {
			
			
			if (handleDraggedFilenames(dropStr, dropRow))
				return true;
			
		} else if (dropStr.startsWith("http:")) {
			
			URL url = new URL(dropStr);
			
			
			return handleDropTransfer(url, dropRow);
		}
		File tmpfile = java.io.File.createTempFile("jabrefimport", "");
		tmpfile.deleteOnExit();
		FileWriter fw = new FileWriter(tmpfile);
		fw.write(dropStr);
		fw.close();

		

		ImportMenuItem importer = new ImportMenuItem(frame, false);
		importer.automatedImport(new String[] { tmpfile.getAbsolutePath() });

		return true;
	}

	
	private boolean handleDraggedFilenames(String s, final int dropRow) {
		
		String[] lines = s.replaceAll("\r", "").split("\n");
		List files = new ArrayList();
		for (int i = 0; i < lines.length; i++) {
			String line = lines[i];
			if (line.startsWith("file:"))
				line = line.substring(5);
			else
				continue;
			
			
			if (line.startsWith("//"))
				line = line.substring(2);
			File f = new File(line);
			if (f.exists()) {
				files.add(f);
			}
		}
		return handleDraggedFiles(files, dropRow);

	}

	
	private boolean handleDraggedFiles(List files, final int dropRow) {
		final String[] fileNames = new String[files.size()];
		int i = 0;
		for (Iterator iterator = files.iterator(); iterator.hasNext();) {
			File file = (File) iterator.next();
			fileNames[i] = file.getAbsolutePath();
			i++;
		}
		
		
		
		new Thread(new Runnable() {
			public void run() {
				loadOrImportFiles(fileNames, dropRow);
			}
		}).start();

		return true;
	}

	
	private void loadOrImportFiles(String[] fileNames, int dropRow) {

		OpenDatabaseAction openAction = new OpenDatabaseAction(frame, false);
		ArrayList notBibFiles = new ArrayList();
		String encoding = Globals.prefs.get("defaultEncoding");
		for (int i = 0; i < fileNames.length; i++) {
			
			String extension = "";
			ExternalFileType fileType = null;
			int index = fileNames[i].lastIndexOf('.');
			if ((index >= 0) && (index < fileNames[i].length())) {
				extension = fileNames[i].substring(index + 1).toLowerCase();
				
				fileType = Globals.prefs.getExternalFileTypeByExt(extension);
			}
			if (extension.equals("bib")) {
				File f = new File(fileNames[i]);
				try {
					ParserResult pr = OpenDatabaseAction.loadDatabase(f, encoding);
					if ((pr == null) || (pr == ParserResult.INVALID_FORMAT)) {
						notBibFiles.add(fileNames[i]);
					} else {
						openAction.addNewDatabase(pr, f, true);
                        frame.getFileHistory().newFile(fileNames[i]);
                    }
				} catch (IOException e) {
					notBibFiles.add(fileNames[i]);
					
					
					
					
					
				}
				continue;
			}

			
			if (fileType != null && dropRow >= 0) {

				
				boolean local = true;

				
				DroppedFileHandler dfh = new DroppedFileHandler(frame, panel);
				dfh.handleDroppedfile(fileNames[i], fileType, local, entryTable, dropRow);

				continue;
			}


			notBibFiles.add(fileNames[i]);
		}

		if (notBibFiles.size() > 0) {
			String[] toImport = new String[notBibFiles.size()];
			notBibFiles.toArray(toImport);

			
			
			ImportMenuItem importer = new ImportMenuItem(frame, (entryTable == null));
			importer.automatedImport(toImport);
		}
	}

	protected boolean handleDropTransfer(URL dropLink, int dropRow) throws IOException {
		File tmpfile = java.io.File.createTempFile("jabrefimport", "");
		tmpfile.deleteOnExit();

		
		

		new URLDownload(entryTable, dropLink, tmpfile).download();

		
		ImportMenuItem importer = new ImportMenuItem(frame, (entryTable == null));
		importer.automatedImport(new String[] { tmpfile.getAbsolutePath() });

		return true;
	}

}