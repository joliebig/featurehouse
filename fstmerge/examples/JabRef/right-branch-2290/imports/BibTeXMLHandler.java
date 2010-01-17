package net.sf.jabref.imports;

import org.xml.sax.*;
import org.xml.sax.helpers.*;
import java.util.*;
import net.sf.jabref.*;



public class BibTeXMLHandler extends DefaultHandler {

    private ArrayList bibitems;

    private BibtexEntry b; 

    
    private String name; 
    private String currentChars;

    public BibTeXMLHandler() {
        super();
    }

    public ArrayList getItems(){ return bibitems;}

    

    public void doctypeDecl(String name, String publicId,
        String systemId) {}

    public void startDocument() {
        bibitems = new ArrayList();
    }

    public void endDocument() {
    }

    public void characters(char ch[], int start, int length) {
        String s = new String(ch, start, length).trim();
        currentChars += s;
    }

    public void startElement(String uri, String local, String raw, Attributes atts) {
        String name = raw;
        this.name = name;
        if (name.equals("bibtex:entry")) {
            String articleID = null;
            for (int i = 0; i < atts.getLength(); i++) {
                if (atts.getQName(i).equals("bibtex:id") ||
                    atts.getQName(i).equals("id")) {
                    articleID = atts.getValue(i);
                }
            }
            b = new BibtexEntry(Util.createNeutralId());
            b.setField(BibtexFields.KEY_FIELD, articleID);
        } else if (
            name.equals("bibtex:article") ||
            name.equals("bibtex:inbook") ||
            name.equals("bibtex:book") ||
            name.equals("bibtex:booklet") ||
            name.equals("bibtex:incollection") ||
            name.equals("bibtex:inproceedings") ||
            name.equals("bibtex:proceedings") ||
            name.equals("bibtex:manual") ||
            name.equals("bibtex:mastersthesis") ||
            name.equals("bibtex:phdthesis") ||
            name.equals("bibtex:techreport") ||
            name.equals("bibtex:unpublished") ||
            name.equals("bibtex:misc") ||
            name.equals("bibtex:other")) {
            BibtexEntryType tp = BibtexEntryType.getType(local);
            b.setType(tp);
        }
        currentChars = "";
    }

    public void endElement(String uri, String local, String raw) {
        String name = raw;
        if (name.equals("bibtex:entry")) {
            bibitems.add( b  );
        } else if (name.startsWith("bibtex:")) {
            b.setField(local, currentChars);
            
        }
        currentChars = "";
    }

}


