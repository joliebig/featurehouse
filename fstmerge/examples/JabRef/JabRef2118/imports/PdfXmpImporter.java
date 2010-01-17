package net.sf.jabref.imports; 

import java.io.IOException; 
import java.io.InputStream; 
import java.util.List; 
import net.sf.jabref.Globals; 
import net.sf.jabref.util.XMPUtil; 

import net.sf.jabref.BibtexEntry; 


public  class  PdfXmpImporter  extends ImportFormat {
	

	public String getFormatName() {
		return Globals.lang("XMP-annotated PDF");
	}


	

	
	public List<BibtexEntry> importEntries(InputStream in) throws IOException {
		return XMPUtil.readXMP(in);
	}


	

	
	public boolean isRecognizedFormat(InputStream in) throws IOException {
		return XMPUtil.hasMetadata(in);
	}


	

	
	public String getCLIid() {
		return "xmp";
	}



}
