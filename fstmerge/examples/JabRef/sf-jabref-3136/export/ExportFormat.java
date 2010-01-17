package net.sf.jabref.export;

import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.Globals;
import net.sf.jabref.MetaData;
import net.sf.jabref.export.layout.Layout;
import net.sf.jabref.export.layout.LayoutHelper;

import javax.swing.filechooser.FileFilter;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.*;


public class ExportFormat implements IExportFormat {

	String displayName;
	String consoleName;
	String lfFileName;
	String directory;
	String extension;
    String encoding = null; 
      

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

    
    protected void setEncoding(String encoding) {
        this.encoding = encoding;
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
        SaveSession ss = null;
        if (this.encoding != null) {
            try {
                ss = getSaveSession(this.encoding, outFile);
            } catch (IOException ex) {
                
                
                ex.printStackTrace();

            }
        }
		if (ss == null)
		    ss = getSaveSession(encoding, outFile);
        
		VerifyingWriter ps = ss.getWriter();

        Layout beginLayout = null;
		Reader reader = null;

        
        HashMap<String,String> customNameFormatters = readFormatterFile(lfFileName);
        
        Globals.prefs.customExportNameFormatters = customNameFormatters;

        ArrayList<String> missingFormatters = new ArrayList<String>(1);

        
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
            missingFormatters.addAll(beginLayout.getMissingFormatters());
		}

		
		List<BibtexEntry> sorted = FileActions.getSortedEntries(database,
			entryIds, false);

		
		reader = getReader(lfFileName + ".layout");

		LayoutHelper layoutHelper = new LayoutHelper(reader);
		Layout defLayout = layoutHelper
			.getLayoutFromText(Globals.FORMATTER_PACKAGE);
		reader.close();
        if (defLayout != null) {
            missingFormatters.addAll(defLayout.getMissingFormatters());
            System.out.println(defLayout.getMissingFormatters());
        }
		HashMap<String, Layout> layouts = new HashMap<String, Layout>();
		Layout layout;

        ExportFormats.entryNumber = 0;
		for (BibtexEntry entry : sorted) {
            ExportFormats.entryNumber++; 
			
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
                    if (layout != null)
                        missingFormatters.addAll(layout.getMissingFormatters());

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
            missingFormatters.addAll(endLayout.getMissingFormatters());
		}

        
        Globals.prefs.customExportNameFormatters = null;

        if (missingFormatters.size() > 0) {
            StringBuilder sb = new StringBuilder("The following formatters could not be found").
                    append(": ");
            for (Iterator<String> i = missingFormatters.iterator(); i.hasNext();) {
                sb.append(i.next());
                if (i.hasNext())
                    sb.append(", ");
            }
            System.err.println(sb.toString());
        }

        finalizeSaveSession(ss);
	}

    
    private HashMap<String, String> readFormatterFile(String lfFileName) {
        HashMap<String,String> formatters = new HashMap<String, String>();
        File formatterFile = new File(lfFileName + ".formatters");
        if (formatterFile.exists()) {
            Reader in = null;
            try {
                in = new FileReader(formatterFile);
                if (in != null) {
                    
                    StringBuilder sb = new StringBuilder();
                    int c;
                    while ((c = in.read()) != -1) {
                        sb.append((char)c);
                    }
                    String[] lines = sb.toString().split("\n");
                    
                    for (int i=0; i<lines.length; i++) {
                        String line = lines[i].trim();
                        
                        if (line.length() == 0)
                            continue;
                        int index = line.indexOf(":"); 
                        if ((index > 0) && (index+1 < line.length())) {
                            String formatterName = line.substring(0, index);
                            String contents = line.substring(index+1);
                            
                            
                            formatters.put(formatterName, contents);
                        }
                    }
                }

            } catch (IOException ex) {
                
                ex.printStackTrace();
            } finally {
                if (in != null)
                    try {
                        in.close();
                    } catch (IOException ex) {
                        ex.printStackTrace();
                    }
            }
        }
        return formatters;
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
