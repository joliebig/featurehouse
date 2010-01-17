package net.sf.jabref.export;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import javax.swing.filechooser.FileFilter;

import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.MetaData;
import net.sf.jabref.Globals;
import net.sf.jabref.export.layout.Layout;
import net.sf.jabref.export.layout.LayoutHelper;


public class ExportFormat implements IExportFormat {

	String displayName;
	String consoleName;
	String lfFileName;
	String directory;
	String extension;
	FileFilter fileFilter;
	boolean customExport = false;

	
	public ExportFormat(String displayName, String consoleName,
		String lfFileName, String directory, String extension) {
		this.displayName = displayName;
		this.consoleName = consoleName;
		this.lfFileName = lfFileName;
		this.directory = directory;
		this.extension = extension;
	}

	
	protected ExportFormat() {
		
	}

	
	public void setCustomExport(boolean custom) {
		this.customExport = custom;
	}

	
	public String getConsoleName() {
		return consoleName;
	}

	
	public String getDisplayName() {
		return displayName;
	}

	
	protected Reader getReader(String filename) throws IOException {
		
		String dir;
		if (customExport) {
			dir = "";
		} else {
			dir = Globals.LAYOUT_PREFIX
				+ (directory == null ? "" : directory + "/");
		}
		return FileActions.getReader(dir + filename);
	}

	
	public void performExport(final BibtexDatabase database,
            final MetaData metaData, final String file,
		final String encoding, Set<String> entryIds) throws Exception {

		File outFile = new File(file);
		SaveSession ss = getSaveSession(encoding, outFile);
		VerifyingWriter ps = ss.getWriter();

		
		Layout beginLayout = null;
		Reader reader;
		try {
			reader = getReader(lfFileName + ".begin.layout");
			LayoutHelper layoutHelper = new LayoutHelper(reader);
			beginLayout = layoutHelper
				.getLayoutFromText(Globals.FORMATTER_PACKAGE);
			reader.close();
		} catch (IOException ex) {
			
			
		}
		
		if (beginLayout != null) {
			ps.write(beginLayout.doLayout(database, encoding));
		}

		
		List<BibtexEntry> sorted = FileActions.getSortedEntries(database,
			entryIds, false);

		
		reader = getReader(lfFileName + ".layout");

		LayoutHelper layoutHelper = new LayoutHelper(reader);
		Layout defLayout = layoutHelper
			.getLayoutFromText(Globals.FORMATTER_PACKAGE);
		reader.close();
		HashMap<String, Layout> layouts = new HashMap<String, Layout>();
		Layout layout;
		for (BibtexEntry entry : sorted) {
			
			String type = entry.getType().getName().toLowerCase();
			if (layouts.containsKey(type))
				layout = layouts.get(type);
			else {
				try {
					
					reader = getReader(lfFileName + "." + type + ".layout");
					layoutHelper = new LayoutHelper(reader);
					layout = layoutHelper
						.getLayoutFromText(Globals.FORMATTER_PACKAGE);
					layouts.put(type, layout);
					reader.close();
				} catch (IOException ex) {
					
					
					
					layout = defLayout;
				}
			}

			
			ps.write(layout.doLayout(entry, database));
		}

		

		
		Layout endLayout = null;
		try {
			reader = getReader(lfFileName + ".end.layout");
			layoutHelper = new LayoutHelper(reader);
			endLayout = layoutHelper
				.getLayoutFromText(Globals.FORMATTER_PACKAGE);
			reader.close();
		} catch (IOException ex) {
			
			
		}

		
		if (endLayout != null) {
			ps.write(endLayout.doLayout(database, encoding));
		}

		finalizeSaveSession(ss);
	}

	protected SaveSession getSaveSession(final String encoding,
		final File outFile) throws IOException {
		return new SaveSession(outFile, encoding, false);
	}

	
	public FileFilter getFileFilter() {
		if (fileFilter == null)
			fileFilter = new ExportFileFilter(this, extension);
		return fileFilter;
	}

	public void finalizeSaveSession(final SaveSession ss) throws Exception {
		ss.getWriter().flush();
		ss.getWriter().close();

		if (!ss.getWriter().couldEncodeAll()) {
			System.err.println("Could not encode...");
		}
		ss.commit();
	}
}
