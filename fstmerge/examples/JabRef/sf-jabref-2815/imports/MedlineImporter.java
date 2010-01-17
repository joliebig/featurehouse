package net.sf.jabref.imports;

import java.io.*;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import net.sf.jabref.BibtexEntry;


public class MedlineImporter extends ImportFormat {

    
    public String getFormatName() {
        return "Medline";
    }

    
    public String getCLIId() {
        return "medline";
    }

    
    public boolean isRecognizedFormat(InputStream stream) throws IOException {

        BufferedReader in = new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));
        String str;
        int i=0;
        while (((str = in.readLine()) != null) && (i < 50)) {

			if (str.toLowerCase().indexOf("<pubmedarticle>") >= 0)
				return true;

            i++;
        }

		return false;
    }

    
    public static List<BibtexEntry> fetchMedline(String id) {
        String baseUrl = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=xml&rettype=citation&id=" +
            id;
        try {
            URL url = new URL(baseUrl);
            URLConnection data = url.openConnection();
            return new MedlineImporter().importEntries(data.getInputStream());
        } catch (IOException e) {
            return new ArrayList<BibtexEntry>();
        }
    }

    
    public List<BibtexEntry> importEntries(InputStream stream) throws IOException {

        
        SAXParserFactory parserFactory = SAXParserFactory.newInstance();

        
        
        parserFactory.setValidating(true);
        parserFactory.setNamespaceAware(true);

        
        ArrayList<BibtexEntry> bibItems = null;
        try {
            SAXParser parser = parserFactory.newSAXParser(); 
            
            MedlineHandler handler = new MedlineHandler();
            
            
            parser.parse(stream, handler);

            
            if (false) {
                stream.reset();
                FileOutputStream out = new FileOutputStream(new File("/home/alver/ut.txt"));
                int c;
                while ((c = stream.read()) != -1) {
                    out.write((char) c);
                }
                out.close();
            }

            
            
            bibItems = handler.getItems();
        } catch (javax.xml.parsers.ParserConfigurationException e1) {
            e1.printStackTrace();
        } catch (org.xml.sax.SAXException e2) {
            e2.printStackTrace();
        } catch (java.io.IOException e3) {
            e3.printStackTrace();
        }

        return bibItems;
    }

}
