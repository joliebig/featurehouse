package net.sf.jabref.imports;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;

import javax.swing.JOptionPane;
import javax.swing.JPanel;

import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.GUIGlobals;
import net.sf.jabref.Globals;
import net.sf.jabref.OutputPrinter;


public class SPIRESFetcher implements EntryFetcher {

	private static String spiresHost = "www-spires.slac.stanford.edu";

	public SPIRESFetcher() {
	}

	
	public String constructUrl(String key) {
		String identifier = "";
		try {
			identifier = URLEncoder.encode(key, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			return "";
		}
		StringBuffer sb = new StringBuffer("http://").append(spiresHost)
				.append("/");
		sb.append("spires/find/hep/www").append("?");
		sb.append("rawcmd=find+");
		sb.append(identifier);
		sb.append("&FORMAT=WWWBRIEFBIBTEX&SEQUENCE=");
		return sb.toString();
	}

	
	public static String constructUrlFromSlaccitation(String slaccitation) {
		String cmd = "j";
		String key = slaccitation.replaceAll("^%%CITATION = ", "").replaceAll(
				";%%$", "");
		if (key.matches("^\\w*-\\w*[ /].*"))
			cmd = "eprint";
		try {
			key = URLEncoder.encode(key, "UTF-8");
		} catch (UnsupportedEncodingException e) {
		}
		StringBuffer sb = new StringBuffer("http://").append(spiresHost)
				.append("/");
		sb.append("spires/find/hep/www").append("?");
		sb.append("rawcmd=find+").append(cmd).append("+");
		sb.append(key);
		return sb.toString();
	}

	
	public static String constructUrlFromEprint(String eprint) {
		String key = eprint.replaceAll(" [.*]$", "");
		try {
			key = URLEncoder.encode(key, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			return "";
		}
		StringBuffer sb = new StringBuffer("http://").append(spiresHost)
				.append("/");
		sb.append("spires/find/hep/www").append("?");
		sb.append("rawcmd=find+eprint+");
		sb.append(key);
		return sb.toString();
	}

	
	private BibtexDatabase importSpiresEntries(String key, OutputPrinter frame) {
		String url = constructUrl(key);
		try {
			HttpURLConnection conn = (HttpURLConnection) (new URL(url)).openConnection();
			conn.setRequestProperty("User-Agent", "Jabref");
			InputStream inputStream = conn.getInputStream();

			SPIRESBibtexFilterReader reader = new SPIRESBibtexFilterReader(
					new InputStreamReader(inputStream));

			ParserResult pr = BibtexParser.parse(reader);

			return pr.getDatabase();
		} catch (IOException e) {
			frame.showMessage( Globals.lang(
					"An Exception ocurred while accessing '%0'", url)
					+ "\n\n" + e.toString(), Globals.lang(getKeyName()),
					JOptionPane.ERROR_MESSAGE);
		} catch (RuntimeException e) {
			frame.showMessage( Globals.lang(
					"An Error occurred while fetching from SPIRES source (%0):",
					new String[] { url })
					+ "\n\n" + e.getMessage(), Globals.lang(getKeyName()),
					JOptionPane.ERROR_MESSAGE);
		}
		return null;
	}

	
	
	
	
	
	
	
	
	
	
	

	
	public String getHelpPage() {
		return "Spires.html";
	}

	public URL getIcon() {
		return GUIGlobals.getIconUrl("www");
	}

	public String getKeyName() {
		return "Fetch SPIRES";
	}

	public JPanel getOptionsPanel() {
		
		return null;
	}

	public String getTitle() {
		return Globals.menuTitle(getKeyName());
	}

	
	public void cancelled() {
	}

	public void done(int entriesImported) {
	}

	public void stopFetching() {
	}

	
	public boolean processQuery(String query, ImportInspector dialog,
								OutputPrinter frame) {
		try {
			frame.setStatus("Fetching entries from Spires");
			
			BibtexDatabase bd = importSpiresEntries(query,frame);

			

			frame.setStatus("Adding fetched entries");
			
			if (bd.getEntryCount() > 0)
		        for (BibtexEntry entry : bd.getEntries())
		        	dialog.addEntry(entry);

			
			
			
		} catch (Exception e) {
			frame.showMessage(Globals.lang("Error while fetching from Spires: ")
					+ e.getMessage());
			e.printStackTrace();
		}
		return true;
	}
}
