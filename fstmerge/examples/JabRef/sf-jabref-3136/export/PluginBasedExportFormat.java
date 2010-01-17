package net.sf.jabref.export;

import java.io.*;
import java.net.URL;

import net.sf.jabref.Globals;
import net.sf.jabref.plugin.core.generated._JabRefPlugin.ExportFormatTemplateExtension;


public class PluginBasedExportFormat extends ExportFormat {

	public ExportFormatTemplateExtension extension;

	
	public static PluginBasedExportFormat getFormat(
		ExportFormatTemplateExtension extension) {

		String consoleName = extension.getConsoleName();
		String displayName = extension.getDisplayName();
		String layoutFilename = extension.getLayoutFilename();
		String fileExtension = extension.getExtension();
        String encoding = extension.getEncoding();
		if ("".equals(fileExtension) || "".equals(displayName)
			|| "".equals(consoleName) || "".equals(layoutFilename)) {
			Globals.logger("Could not load extension " + extension.getId());
			return null;
		}

		return new PluginBasedExportFormat(displayName, consoleName,
			layoutFilename, fileExtension, encoding, extension);
	}

	public PluginBasedExportFormat(String displayName, String consoleName,
		String layoutFileName, String fileExtension, String encoding,
		ExportFormatTemplateExtension extension) {
		super(displayName, consoleName, layoutFileName, null, fileExtension);
        
        if (encoding != null)
            setEncoding(encoding);
		this.extension = extension;
	}

	@Override
	public Reader getReader(String filename) throws IOException {
		URL reso = extension.getDirAsUrl(filename);

		if (reso != null) {
			try {
				return new InputStreamReader(reso.openStream());
			} catch (FileNotFoundException ex) {
				
			}
		}

		try {
			return new FileReader(new File(filename));
		} catch (FileNotFoundException ex) {
			
		}
		throw new IOException(Globals.lang("Could not find layout file")
			+ ": '" + filename + "'.");
	}
}
