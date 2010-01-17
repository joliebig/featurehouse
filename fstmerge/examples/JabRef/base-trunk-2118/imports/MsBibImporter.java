package net.sf.jabref.imports;

import java.util.List;
import java.util.ArrayList;
import java.io.InputStream;
import java.io.IOException;
import net.sf.jabref.msbib.*;
import javax.xml.parsers.*;
import org.w3c.dom.*;



public class MsBibImporter extends ImportFormat {

    public boolean isRecognizedFormat(InputStream in) throws IOException {

        
    	Document docin = null;
    	try {
    	DocumentBuilder dbuild = DocumentBuilderFactory.
    								newInstance().
    								newDocumentBuilder();
   		docin = dbuild.parse(in);   		
    	} catch (Exception e) {
	   		return false;
    	}
    	if(docin!= null && docin.getDocumentElement().getTagName().contains("Sources") == false)
    		return false;





    	
        return true;
    }

    
	public String getCLIid() {
		return "msbib";
	}

    public List importEntries(InputStream in) throws IOException {

        MSBibDatabase dbase = new MSBibDatabase();

        List entries = dbase.importEntries(in);

        return entries;
    }

    public String getFormatName() {
        
        return "MSBib";
    }

}
