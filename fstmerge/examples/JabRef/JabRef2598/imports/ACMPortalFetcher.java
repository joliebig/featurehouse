

package net.sf.jabref.imports; 

import java.awt.BorderLayout; 
import java.io.BufferedInputStream; 
import java.io.BufferedReader; 
import java.io.File; 
import java.io.FileInputStream; 
import java.io.IOException; 
import java.io.InputStream; 
import java.io.InputStreamReader; 
import java.net.ConnectException; 
import java.net.MalformedURLException; 
import java.net.URL; 
import java.util.Collection; 
import java.util.regex.Matcher; 
import java.util.regex.Pattern; 

import javax.swing.ButtonGroup; 
import javax.swing.JCheckBox; 
import javax.swing.JOptionPane; 
import javax.swing.JPanel; 
import javax.swing.JRadioButton; 

import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.GUIGlobals; 
import net.sf.jabref.Globals; 
import net.sf.jabref.OutputPrinter; 


public  class  ACMPortalFetcher implements  EntryFetcher {
	

	ImportInspector dialog = null;

	
	OutputPrinter status;

	
    HTMLConverter htmlConverter = new HTMLConverter();

	
    private String terms;

	
    String startUrl = "http://portal.acm.org/";

	
    String searchUrlPart = "results.cfm?query=";

	
    String searchUrlPartII = "&dl=";

	
    String endUrl = "&coll=Portal&short=1";

	

    private static final int MAX_FETCH = 50;

	 
    private int perPage = MAX_FETCH, hits = 0, unparseable = 0, parsed = 0;

	
    private boolean shouldContinue = false;

	
    private JRadioButton acmButton = new JRadioButton(Globals.lang("The ACM Digital Library"));

	
    private JRadioButton guideButton = new JRadioButton(Globals.lang("The Guide to Computing Literature"));

	
    private JCheckBox fetchAstracts = new JCheckBox(Globals.lang("Include abstracts"), false);

	
    private boolean fetchingAbstracts = false;

	
    private boolean acmOrGuide = false;

	

    Pattern hitsPattern = Pattern.compile(".*Found <b>(\\d+,*\\d*)</b> of.*");

	
    ~~FSTMerge~~ Pattern maxHitsPattern = Pattern.compile(".*Results \\d+ - \\d+ of (\\d+,*\\d*).*"); ##FSTMerge## ##FSTMerge## Pattern maxHitsPattern = Pattern.compile(".*<td>Results \\d+ - \\d+ of (\\d+,*\\d*)</td>.*");

	
    Pattern risPattern = Pattern.compile(".*(popBibTex.cfm.*)','BibTex'.*");

	
    Pattern absPattern = Pattern.compile(".*ABSTRACT</A></span>\\s+<p class=\"abstract\">\\s+(.*)");

	
    
    Pattern fullCitationPattern =
        Pattern.compile("<A HREF=\"(citation.cfm.*)\" class.*");

	

    public JPanel getOptionsPanel() {
        JPanel pan = new JPanel();
        pan.setLayout(new BorderLayout());

        acmButton.setSelected(true);
        
        ButtonGroup group = new ButtonGroup();
        group.add(acmButton);
        group.add(guideButton);
        pan.add(fetchAstracts, BorderLayout.NORTH);
        pan.add(acmButton, BorderLayout.CENTER);
        pan.add(guideButton, BorderLayout.SOUTH);

        return pan;
    }


	

    <<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_47245
public boolean processQuery(String query, ImportInspector dialog, OutputPrinter status) {
        this.dialog = dialog;
        this.status = status;
        this.terms = query;
        piv = 0;
        shouldContinue = true;
        parsed = 0;
        unparseable = 0;
        acmOrGuide = acmButton.isSelected();
        String address = makeUrl(0);
        try {
            URL url = new URL(address);

            
            String page = getResults(url);
            
            hits = getNumberOfHits(page, "Found", hitsPattern);
			int index = page.indexOf("Found");
			if (index >= 0) {
            	page = page.substring(index + 5);
				index = page.indexOf("Found");
				if (index >= 0)
            		page = page.substring(index);
			}
            
            
            
            if (hits == 0) {
                status.showMessage(Globals.lang("No entries found for the search string '%0'",
                        terms),
                        Globals.lang("Search ACM Portal"), JOptionPane.INFORMATION_MESSAGE);
                return false;
            }

            int maxHits = getNumberOfHits(page, "Results", maxHitsPattern);
            
            

            
            
            
            if (hits > maxHits)
                hits = maxHits;
            
            if (hits > MAX_FETCH) {
                status.showMessage(Globals.lang("%0 entries found. To reduce server load, "
                        +"only %1 will be downloaded.",
                                new String[] {String.valueOf(hits), String.valueOf(MAX_FETCH)}),
                        Globals.lang("Search ACM Portal"), JOptionPane.INFORMATION_MESSAGE);
                hits = MAX_FETCH;
            }
        
            fetchingAbstracts = fetchAstracts.isSelected();
            
            
            parse(dialog, page, 0, 1);
            
            int firstEntry = perPage;
            while (shouldContinue && (firstEntry < hits)) {
                
                address = makeUrl(firstEntry);
                
                page = getResults(new URL(address));
                
                
                if (!shouldContinue)
                    break;

                parse(dialog, page, 0, 1+firstEntry);
                firstEntry += perPage;
            }
            return true;
        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (ConnectException e) {
            status.showMessage(Globals.lang("Connection to ACM Portal failed"),
                    Globals.lang("Search ACM Portal"), JOptionPane.ERROR_MESSAGE);
        } catch (IOException e) {
        	status.showMessage(Globals.lang(e.getMessage()),
                    Globals.lang("Search ACM Portal"), JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
        }
        return false;
    }
=======
public boolean processQuery(String query, ImportInspector dialog, OutputPrinter status) {
        this.dialog = dialog;
        this.status = status;
        this.terms = query;
        piv = 0;
        shouldContinue = true;
        parsed = 0;
        unparseable = 0;
        acmOrGuide = acmButton.isSelected();
        String address = makeUrl(0);
        try {
            URL url = new URL(address);

            
            String page = getResults(url);
            
            hits = getNumberOfHits(page, "Found", hitsPattern);
            
            
            
            if (hits == 0) {
                status.showMessage(Globals.lang("No entries found for the search string '%0'",
                        terms),
                        Globals.lang("Search ACM Portal"), JOptionPane.INFORMATION_MESSAGE);
                return false;
            }

            int maxHits = getNumberOfHits(page, "<td>Results", maxHitsPattern);
            
            

            
            
            
            if (hits > maxHits)
                hits = maxHits;
            
            if (hits > MAX_FETCH) {
                status.showMessage(Globals.lang("%0 entries found. To reduce server load, "
                        +"only %1 will be downloaded.",
                                new String[] {String.valueOf(hits), String.valueOf(MAX_FETCH)}),
                        Globals.lang("Search ACM Portal"), JOptionPane.INFORMATION_MESSAGE);
                dialog.toFront(); 
                hits = MAX_FETCH;
            }
        
            fetchingAbstracts = fetchAstracts.isSelected();
            
            
            parse(dialog, page, 0, 1);
            
            int firstEntry = perPage;
            while (shouldContinue && (firstEntry < hits)) {
                
                address = makeUrl(firstEntry);
                
                page = getResults(new URL(address));
                
                
                if (!shouldContinue)
                    break;

                parse(dialog, page, 0, 1+firstEntry);
                firstEntry += perPage;
            }
            return true;
        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (ConnectException e) {
            status.showMessage(Globals.lang("Connection to ACM Portal failed"),
                    Globals.lang("Search ACM Portal"), JOptionPane.ERROR_MESSAGE);
        } catch (IOException e) {
        	status.showMessage(Globals.lang(e.getMessage()),
                    Globals.lang("Search ACM Portal"), JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
        }
        return false;
    }
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_47247


	

    private String makeUrl(int startIndex) {
        StringBuffer sb = new StringBuffer(startUrl).append(searchUrlPart);
        sb.append(terms.replaceAll(" ", "%20"));
        sb.append(searchUrlPartII);
        if (acmOrGuide)
        	sb.append("ACM");
        else
        	sb.append("GUIDE");
        sb.append(endUrl);
        return sb.toString();
    }


	

    int piv = 0;

	

    private void parse(ImportInspector dialog, String text, int startIndex, int firstEntryNumber) {
        piv = startIndex;
        int entryNumber = firstEntryNumber;
        BibtexEntry entry;
        while (((entry = parseNextEntry(text, piv, entryNumber)) != null)
            && (shouldContinue)) {
            if (entry.getField("title") != null) {
                dialog.addEntry(entry);
                dialog.setProgress(parsed + unparseable, hits);
                parsed++;
            }
            entryNumber++;
            
        }
    }


	

    private BibtexEntry parseEntryBibTeX(String fullCitation, boolean abs)
        throws IOException
    {
        URL url;
        try {
            url = new URL(startUrl + fullCitation);
        	String page = getResults(url);
			Matcher bibtexAddr = risPattern.matcher(page);
			if (bibtexAddr.find()) {
				URL bibtexUrl = new URL(startUrl + bibtexAddr.group(1));
				BufferedReader in = new BufferedReader(new InputStreamReader(bibtexUrl.openStream()));
				ParserResult result = BibtexParser.parse(in);
				in.close();
				Collection<BibtexEntry> item = result.getDatabase().getEntries();
				BibtexEntry entry = item.iterator().next();
				if (abs == true) {
					Matcher absMatch = absPattern.matcher(page);
					if (absMatch.find()) {
						String absBlock = absMatch.group(1);
						entry.setField("abstract", convertHTMLChars(absBlock).trim());
					} else {
						System.out.println("No abstract matched.");
						
					}
				}
				return entry;
			} else
				return null;
        } catch (MalformedURLException e) {
            e.printStackTrace();
            return null;
        } catch (ConnectException e) {
            e.printStackTrace();
            return null;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
    }


	

    private BibtexEntry parseNextEntry(String allText, int startIndex, int entryNumber)
    {
        String toFind = new StringBuffer().append("<strong>")
                .append(entryNumber).append("</strong>").toString();
        int index = allText.indexOf(toFind, startIndex);
        int endIndex = allText.indexOf("</table>", index+1);
        
            endIndex = allText.length();

        BibtexEntry entry = null;

        if (index >= 0) {
            piv = index+1;
            String text = allText.substring(index, endIndex);
            
			Matcher fullCitation =
				fullCitationPattern.matcher(text);
			if (fullCitation.find()) {
				try {
					entry = parseEntryBibTeX(fullCitation.group(1), fetchingAbstracts);
				} catch (IOException e) {
					e.printStackTrace();  
				}
			} else {
				System.out.printf("Citation Unmatched %d\n", entryNumber);
				System.out.printf(text);
			}
            if (entry != null) { 
                return entry;
            }
        }
        
        
        
        return null;
    }


	

    
    private String convertHTMLChars(String text) {

        return htmlConverter.format(text);
    }


	


    
    <<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_47263
private int getNumberOfHits(String page, String marker, Pattern pattern) throws IOException {
        int ind = page.indexOf(marker);
        if (ind < 0) {
        	System.out.println(page);
            throw new IOException(Globals.lang("Could not parse number of hits"));
        }
        String substring = page.substring(ind, Math.min(ind + 42, page.length()));
        Matcher m = pattern.matcher(substring);
        if (!m.find()) {
        	System.out.println("Unmatched!");
        	System.out.println(substring);
        } else {
            try {
            	
            	String number = m.group(1);
            	
            	
            	number = number.replaceAll(",", "");
            	
                return Integer.parseInt(number);
            } catch (NumberFormatException ex) {
                throw new IOException(Globals.lang("Could not parse number of hits"));
            } catch (IllegalStateException e) {
                throw new IOException(Globals.lang("Could not parse number of hits"));
            }
        }
        throw new IOException(Globals.lang("Could not parse number of hits"));
    }
=======
private int getNumberOfHits(String page, String marker, Pattern pattern) throws IOException {
        int ind = page.indexOf(marker);
        if (ind < 0)
            throw new IOException(Globals.lang("Could not parse number of hits"));
        String substring = page.substring(ind, Math.min(ind + 42, page.length()));
        Matcher m = pattern.matcher(substring);
        if (!m.find()) {
        	System.out.println("Unmatched!");
        	
        } else if (m.groupCount() >= 1) {
            try {
            	
            	String number = m.group(1);
            	
            	
            	number = number.replaceAll(",", "");
            	
                return Integer.parseInt(number);
            } catch (NumberFormatException ex) {
                throw new IOException(Globals.lang("Could not parse number of hits"));
            } catch (IllegalStateException e) {
                throw new IOException(Globals.lang("Could not parse number of hits"));
            }
        }
        throw new IOException(Globals.lang("Could not parse number of hits"));
    }
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_47265


	

    
    public String getResults(URL source) throws IOException {
        
        InputStream in = source.openStream();
        StringBuffer sb = new StringBuffer();
        byte[] buffer = new byte[256];
        while(true) {
            int bytesRead = in.read(buffer);
            if(bytesRead == -1) break;
            for (int i=0; i<bytesRead; i++)
                sb.append((char)buffer[i]);
        }
        return sb.toString();
    }


	

    
    public String getResultsFromFile(File f) throws IOException {
        InputStream in = new BufferedInputStream(new FileInputStream(f));
        StringBuffer sb = new StringBuffer();
        byte[] buffer = new byte[256];
        while(true) {
            int bytesRead = in.read(buffer);
            if(bytesRead == -1) break;
            for (int i=0; i<bytesRead; i++)
                sb.append((char)buffer[i]);
        }
        return sb.toString();
    }


	

	public String getTitle() {
	    return Globals.menuTitle("Search ACM Portal");
	}


	
	
	
	public URL getIcon() {
	    return GUIGlobals.getIconUrl("www");
	}


	
	
	public String getHelpPage() {
	    return "ACMPortalHelp.html";
	}


	
	
	public String getKeyName() {
	    return "Search ACM Portal";
	}


	
	
	
	public void cancelled() {
	    shouldContinue = false;
	}


	
	
	
	
	
	public void done(int entriesImported) {
	    
	    
	}


	
	
	
	
	
	public void stopFetching() {
	    shouldContinue = false;
	}



}
