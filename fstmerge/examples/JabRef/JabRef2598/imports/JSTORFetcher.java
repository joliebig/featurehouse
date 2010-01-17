package net.sf.jabref.imports; 

import java.io.BufferedReader; 
import java.io.IOException; 
import java.io.InputStreamReader; 
import java.io.UnsupportedEncodingException; 
import java.net.MalformedURLException; 
import java.net.URL; 
import java.net.URLConnection; 
import java.net.URLEncoder; 
import java.util.Collection; 
import java.util.StringTokenizer; 

import javax.swing.JOptionPane; 
import javax.swing.JPanel; 

import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.GUIGlobals; 
import net.sf.jabref.Globals; 
import net.sf.jabref.OutputPrinter; 


public  class  JSTORFetcher implements  EntryFetcher {
	

    
    protected static int MAX_CITATIONS = 200;

	

    
    protected static final String COOKIE_TICKET = "Jstor_Ticket";

	

    
    protected static final String URL_TICKET = "http://www.jstor.org/search";

	

    
    protected static final String COOKIE_CITATIONS = "Jstor_citations0";

	

    
    protected static final String URL_BIBTEX = "http://www.jstor.org/browse/citations.txt?exportFormat=bibtex&exportAction=Display&frame=noframe&dpi=3&config=jstor&viewCitations=1&View=View";

	

    public String getHelpPage() {
        return "JSTOR.html";
    }


	

    public URL getIcon() {
        return GUIGlobals.getIconUrl("www");
    }


	

    public String getKeyName() {
        return "Search JSTOR";
    }


	

    public JPanel getOptionsPanel() {
        
        return null;
    }


	

    public String getTitle() {
        return Globals.menuTitle("Search JSTOR");
    }


	
    
    public void stopFetching() {
        
    }


	

    public boolean processQuery(String query, ImportInspector dialog, OutputPrinter status) {

        try {
            
            String ticket = openTicket();

            
            String citations = getCitations(ticket, query);

            
            Collection<BibtexEntry> entries = getBibtexEntries(ticket, citations);
            
            if (entries.size() == 0){
                status.showMessage(Globals.lang("No entries found for the search string '%0'",
                        query),
                        Globals.lang("Search JSTOR"), JOptionPane.INFORMATION_MESSAGE);
                return false;
            }
            
            for (BibtexEntry entry : entries){
                dialog.addEntry(entry);
            }
            return true;
        } catch (IOException e) {
            status.showMessage(Globals.lang("Error while fetching from JSTOR") + ": " + e.getMessage());
        }
        return false;
    }


	

    
    protected Collection<BibtexEntry> getBibtexEntries(String ticket, String citations)
        throws IOException {
        try {
            URL url = new URL(URL_BIBTEX);
            URLConnection conn = url.openConnection();
            conn.setRequestProperty("Cookie", ticket + "; " + citations);
            conn.connect();

            BibtexParser parser = new BibtexParser(new BufferedReader(new InputStreamReader(conn
                .getInputStream())));
            return parser.parse().getDatabase().getEntries();
        } catch (MalformedURLException e) {
            
            throw new RuntimeException(e);
        }
    }


	

    
    protected String openTicket() throws IOException {
        URL url = new URL(URL_TICKET);
        URLConnection conn = url.openConnection();
        return getCookie(COOKIE_TICKET, conn);
    }


	

    
    protected String getCitations(String ticket, String query) throws IOException {
        String urlQuery;
        try {
            urlQuery = "http://www.jstor.org/search/BasicResults?hp=" + MAX_CITATIONS +
                "&si=1&gw=jtx&jtxsi=1&jcpsi=1&artsi=1&Query=" + URLEncoder.encode(query, "UTF-8") +
                "&wc=on&citationAction=saveAll";
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }

        URL url = new URL(urlQuery);
        URLConnection conn = url.openConnection();
        conn.setRequestProperty("Cookie", ticket);
        return getCookie(COOKIE_CITATIONS, conn);
    }


	

    
    public static String getCookie(String name, URLConnection conn) throws IOException {

        for (int i = 0;; i++) {
            String headerName = conn.getHeaderFieldKey(i);
            String headerValue = conn.getHeaderField(i);

            if (headerName == null && headerValue == null) {
                
                break;
            }
            if (headerName != null && headerName.equals("Set-Cookie")) {
                if (headerValue.startsWith(name)) {
                    
                    StringTokenizer st = new StringTokenizer(headerValue, "; ");
                    while (st.hasMoreElements()) {
                        String token = st.nextToken();
                        if (token.startsWith(name)) {
                            return token;
                        }
                    }
                }
            }

        }
        return null;
    }



}
