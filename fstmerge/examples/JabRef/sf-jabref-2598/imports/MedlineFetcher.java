package net.sf.jabref.imports;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JOptionPane;
import javax.swing.JPanel;

import net.sf.jabref.BibtexEntry;
import net.sf.jabref.GUIGlobals;
import net.sf.jabref.Globals;
import net.sf.jabref.OutputPrinter;


public class MedlineFetcher implements EntryFetcher {

    protected class SearchResult {

        public int count = 0;

        public int retmax = 0;

        public int retstart = 0;

        public String ids = "";

        public void addID(String id) {
            if (ids.equals(""))
                ids = id;
            else
                ids += "," + id;
        }
    }

    
    public static final int PACING = 20;

    boolean shouldContinue;

    OutputPrinter frame;

    ImportInspector dialog;

    public String toSearchTerm(String in) {
        Pattern part1 = Pattern.compile(", ");
        Pattern part2 = Pattern.compile(",");
        Pattern part3 = Pattern.compile(" ");
        Matcher matcher;
        matcher = part1.matcher(in);
        in = matcher.replaceAll("\\+AND\\+");
        matcher = part2.matcher(in);
        in = matcher.replaceAll("\\+AND\\+");
        matcher = part3.matcher(in);
        in = matcher.replaceAll("+");

        return in;
    }

    
    public SearchResult getIds(String term, int start, int pacing) {

        String baseUrl = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils";
        String medlineUrl = baseUrl + "/esearch.fcgi?db=pubmed&retmax=" + Integer.toString(pacing) +
            "&retstart=" + Integer.toString(start) + "&term=";
        
        Pattern idPattern = Pattern.compile("<Id>(\\d+)</Id>");
        Pattern countPattern = Pattern.compile("<Count>(\\d+)<\\/Count>");
        Pattern retMaxPattern = Pattern.compile("<RetMax>(\\d+)<\\/RetMax>");
        Pattern retStartPattern = Pattern.compile("<RetStart>(\\d+)<\\/RetStart>");

        boolean doCount = true;
        SearchResult result = new SearchResult();
        try {
            URL ncbi = new URL(medlineUrl + term);
            
            BufferedReader in = new BufferedReader(new InputStreamReader(ncbi.openStream()));
            String inLine;
            while ((inLine = in.readLine()) != null) {

                
                Matcher idMatcher = idPattern.matcher(inLine);
                if (idMatcher.find()) {
                    result.addID(idMatcher.group(1));
                }
                Matcher retMaxMatcher = retMaxPattern.matcher(inLine);
                if (idMatcher.find()) {
                    result.retmax = Integer.parseInt(retMaxMatcher.group(1));
                }
                Matcher retStartMatcher = retStartPattern.matcher(inLine);
                if (retStartMatcher.find()) {
                    result.retstart = Integer.parseInt(retStartMatcher.group(1));
                }
                Matcher countMatcher = countPattern.matcher(inLine);
                if (doCount && countMatcher.find()) {
                    result.count = Integer.parseInt(countMatcher.group(1));
                    doCount = false;
                }
            }
        } catch (MalformedURLException e) { 
            System.out.println("bad url");
            e.printStackTrace();
        } catch (IOException e) { 
            System.out.println("connection failed");
            e.printStackTrace();

        }
        return result;
    }

    public void stopFetching() {
        shouldContinue = false;
    }

    public String getHelpPage() {
        return GUIGlobals.medlineHelp;
    }

    public URL getIcon() {
        return GUIGlobals.getIconUrl("www");
    }

    public String getKeyName() {
        return "Fetch Medline";
    }

    public JPanel getOptionsPanel() {
        
        return null;
    }

    public String getTitle() {
        return Globals.menuTitle("Search Medline");
    }

    public boolean processQuery(String query, ImportInspector dialog, OutputPrinter frame) {

        shouldContinue = true;

        query = query.trim().replace(';', ',');

        if (query.matches("\\d+[,\\d+]*")) {
            frame.setStatus(Globals.lang("Fetching Medline by id..."));

            List<BibtexEntry> bibs = MedlineImporter.fetchMedline(query);

            if (bibs.size() == 0) {
            	frame.showMessage(Globals.lang("No references found"));
            }
            
            for (BibtexEntry entry : bibs){
                dialog.addEntry(entry);
            }
            return true;
        }

        if (query.length() > 0) {
            frame.setStatus(Globals.lang("Fetching Medline by term..."));

            String searchTerm = toSearchTerm(query);

            
            SearchResult result = getIds(searchTerm, 0, 1);

            if (result.count == 0) {
            	frame.showMessage(Globals.lang("No references found"));
                return false;
            }

            int numberToFetch = result.count;
            if (numberToFetch > PACING) {
                
                while (true) {
                    String strCount = JOptionPane.showInputDialog(Globals.lang("References found") +
                        ": " + numberToFetch + "  " +
                        Globals.lang("Number of references to fetch?"), Integer
                        .toString(numberToFetch));

                    if (strCount == null) {
                        frame.setStatus(Globals.lang("Medline import canceled"));
                        return false;
                    }

                    try {
                        numberToFetch = Integer.parseInt(strCount.trim());
                        break;
                    } catch (RuntimeException ex) {
                        frame.showMessage(Globals.lang("Please enter a valid number"));
                    }
                }
            }

            for (int i = 0; i < numberToFetch; i += PACING) {
                if (!shouldContinue)
                    break;

                int noToFetch = Math.min(PACING, numberToFetch - i);
                
                
                result = getIds(searchTerm, i, noToFetch);

                List<BibtexEntry> bibs = MedlineImporter.fetchMedline(result.ids);
                for (BibtexEntry entry : bibs){
                    dialog.addEntry(entry);
                }
                dialog.setProgress(i + noToFetch, numberToFetch);
            }
            return true;
        }
        frame.showMessage(Globals
            .lang("Please enter a comma separated list of Medline IDs (numbers) or search terms."),
            Globals.lang("Input error"), JOptionPane.ERROR_MESSAGE);
        return false;
    }
}
