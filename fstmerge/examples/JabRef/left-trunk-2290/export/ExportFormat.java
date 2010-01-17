package net.sf.jabref.export;

import net.sf.jabref.Globals;
import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.export.layout.Layout;
import net.sf.jabref.export.layout.LayoutHelper;

import javax.swing.filechooser.FileFilter;
import java.util.List;
import java.util.HashMap;
import java.util.Set;
import java.io.File;
import java.io.Reader;
import java.io.IOException;
import java.io.Writer;


public class ExportFormat implements IExportFormat {

	private String displayName;
	private String consoleName;
	private String lfFileName;
	private String directory;
	private String extension;
	private FileFilter fileFilter;
	private boolean customExport = false;

	
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

	
	public void performExport(final BibtexDatabase database, final String file,
			final String encoding, Set<String> entryIds) throws Exception {

		File outFile = new File(file);
		SaveSession ss = getSaveSession(encoding, outFile);
		VerifyingWriter ps = ss.getWriter();

		performExport(database, entryIds, ps);

		finalizeSaveSession(ss);
	}

	
	public void performExport(final BibtexDatabase database,
			Set<String> entryIds, Writer writer) throws Exception, IOException {
		
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
			writer.write(beginLayout.doLayout(database));
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

			
			writer.write(layout.doLayout(entry, database));
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
			writer.write(endLayout.doLayout(database));
		}
	}

	protected SaveSession getSaveSession(final String encoding,
			final File outFile) throws IOException {
		SaveSession ss = new SaveSession(outFile, encoding, false);
		return ss;
	}

	protected void finalizeSaveSession(final SaveSession ss) throws Exception,
			SaveException {
		ss.getWriter().flush();
		ss.getWriter().close();

		if (!ss.getWriter().couldEncodeAll()) {
			System.err.println("Could not encode...");
		}
		ss.commit();

	}

	
	public FileFilter getFileFilter() {
		if (fileFilter == null)
			fileFilter = new ExportFileFilter(this, extension);
		return fileFilter;
	}


}
