

package net.sf.jabref.msbib; 
import net.sf.jabref.*; 
import java.util.*; 
import java.io.InputStream; 
import java.io.IOException; 
import javax.xml.parsers.*; 
import org.w3c.dom.*; 

import javax.xml.parsers.DocumentBuilder; 
import javax.xml.parsers.DocumentBuilderFactory; 

import net.sf.jabref.BibtexDatabase; 
import net.sf.jabref.BibtexEntry; 

import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.Node; 
import org.w3c.dom.NodeList; 

public  class  MSBibDatabase {
	
	

	
	
	public MSBibDatabase() {
		
		entries = new HashSet<MSBibEntry>();
	}


	
	
	<<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_72943
public MSBibDatabase() {
		
		entries = new HashSet<MSBibEntry>();
	}
=======
public MSBibDatabase(InputStream stream) throws IOException {
		importEntries(stream);
    }
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_72945


	
	
	<<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_72946
public MSBibDatabase() {
		
		entries = new HashSet<MSBibEntry>();
	}
=======
public MSBibDatabase(BibtexDatabase bibtex) {
		Set keySet = bibtex.getKeySet();
        addEntries(bibtex, keySet);
    }
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_72948


	
	
	<<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_72949
public MSBibDatabase() {
		
		entries = new HashSet<MSBibEntry>();
	}
=======
public MSBibDatabase(BibtexDatabase bibtex, Set keySet) {
        if (keySet == null)
            keySet = bibtex.getKeySet();
        addEntries(bibtex, keySet);
    }
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_72951


	

    public List<BibtexEntry> importEntries(InputStream stream) throws IOException {
    	entries = new HashSet<MSBibEntry>();	
    	ArrayList<BibtexEntry> bibitems = new ArrayList<BibtexEntry>();
    	Document docin = null;
    	try {
    	DocumentBuilder dbuild = DocumentBuilderFactory.
    								newInstance().
    								newDocumentBuilder();
   		docin = dbuild.parse(stream);
    	} catch (Exception e) {
	   		System.out.println("Exception caught..." + e);
	   		e.printStackTrace();
    	}
   		String bcol = "b:";
   		NodeList rootLst = docin.getElementsByTagName("b:Sources");
   		if(rootLst.getLength()==0) {   			
   			rootLst = docin.getElementsByTagName("Sources");
   			bcol = "";
   		}
   		if(rootLst.getLength()==0)
   			return bibitems;



   		NodeList sourceList = ((Element)(rootLst.item(0))).getElementsByTagName(bcol+"Source");
   		for(int i=0; i<sourceList.getLength(); i++) {
   			MSBibEntry entry = new MSBibEntry((Element)sourceList.item(i),bcol);
   			entries.add(entry);
   			bibitems.add(entry.getBibtexRepresentation());   			
   		}
   		
   		return bibitems;
    }


	

    


	
	public Document getDOMrepresentation() {
		Document result = null;
	   	try {
	   		DocumentBuilder dbuild = DocumentBuilderFactory.
														newInstance().
														newDocumentBuilder();
	   		result = dbuild.newDocument();
	   		Element msbibCollection = result.createElement("b:Sources");
	   		msbibCollection.setAttribute("SelectedStyle","");
	   		msbibCollection.setAttribute("xmlns", "http://schemas.openxmlformats.org/officeDocument/2006/bibliography");
	   		msbibCollection.setAttribute("xmlns:b", "http://schemas.openxmlformats.org/officeDocument/2006/bibliography");	   			   		 
	   		
	   		for(Iterator<MSBibEntry> iter = entries.iterator(); iter.hasNext(); ) {
	   			MSBibEntry entry = iter.next();
	   			Node node = entry.getDOMrepresentation(result);
	   			msbibCollection.appendChild(node);
	   		}
	   		
	   		result.appendChild(msbibCollection);	   		
	   	}
	   	catch (Exception e)
		{
	   		System.out.println("Exception caught..." + e);
	   		e.printStackTrace();
		}
	   	return result;
	   }


	
	protected Set<MSBibEntry> entries;

	

    private void addEntries(BibtexDatabase database, Set<String> keySet) {
        entries = new HashSet<MSBibEntry>();
        for (String s : keySet){
        	BibtexEntry entry = database.getEntryById(s);
			MSBibEntry newMods = new MSBibEntry(entry);
			entries.add(newMods);
		}
	}


}
