package net.sf.jabref.imports;

import java.io.IOException;
import java.io.InputStream;
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
    
    
    public boolean isRecognizedFormat(InputStream in) throws IOException {
	return true;
    }

    
    public List<BibtexEntry> importEntries(InputStream stream) throws IOException {

	
	SAXParserFactory parserFactory = SAXParserFactory.newInstance();

	
	
	parserFactory.setValidating(true);
	parserFactory.setNamespaceAware(true);
	
	
	ArrayList<BibtexEntry> bibItems = null;
	try{
	    SAXParser parser = parserFactory.newSAXParser(); 
	    MedlineHandler handler = new MedlineHandler();
	    
	    parser.parse(stream, handler);
	    
	    
	    bibItems = handler.getItems();
	}catch (javax.xml.parsers.ParserConfigurationException e1){
	}catch (org.xml.sax.SAXException e2){
	}catch (java.io.IOException e3){
	}
	
	return bibItems;
	
    }
    
}
