package net.sf.jabref.imports; 
import java.util.regex.Pattern; 

import java.io.BufferedReader; 
import java.io.InputStream; 

import javax.xml.parsers.SAXParser; 
import javax.xml.parsers.SAXParserFactory; 
import java.io.IOException; 
import java.util.List; 
import java.util.ArrayList; 

import net.sf.jabref.BibtexEntry; 


public  class  BibteXMLImporter  extends ImportFormat {
	

    
    public String getFormatName() {
	return "BibTeXML";
    }


	

    
    public String getCLIId() {
      return "bibtexml";
    }


	
    

    
    public boolean isRecognizedFormat(InputStream stream) throws IOException {

	
	BufferedReader in = new BufferedReader(ImportFormatReader.getReaderDefaultEncoding(stream));
	Pattern pat1 = Pattern
	    .compile("<bibtex:file .*");
	String str;
	while ((str = in.readLine()) != null){
	    if (pat1.matcher(str).find())
		return true;
	}
	return false;
    }


	

    
    public List<BibtexEntry> importEntries(InputStream stream) throws IOException {

	ArrayList<BibtexEntry> bibItems = new ArrayList<BibtexEntry>();

	
	SAXParserFactory parserFactory = SAXParserFactory.newInstance();
	
	
	
	parserFactory.setNamespaceAware(true);	
	


	try{
	    SAXParser parser = parserFactory.newSAXParser(); 
	    BibTeXMLHandler handler = new BibTeXMLHandler();
	    
	    parser.parse(stream, handler);
	    
	    bibItems = handler.getItems();
	    
	}catch (javax.xml.parsers.ParserConfigurationException e1){
	    e1.printStackTrace();
	}catch (org.xml.sax.SAXException e2){
	    e2.printStackTrace();
	}catch (java.io.IOException e3){
	    e3.printStackTrace();
	}
	return bibItems;
	
    }



}
