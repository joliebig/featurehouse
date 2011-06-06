

package com.lowagie.text.xml.xmp;

import com.lowagie.text.pdf.PdfWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Iterator;
import java.util.Map;

import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfString;


public class XmpWriter {

    
    public static final String UTF8 = "UTF-8";
    
    public static final String UTF16 = "UTF-16";
    
    public static final String UTF16BE = "UTF-16BE";
    
    public static final String UTF16LE = "UTF-16LE";
    
    
    public static final String EXTRASPACE = "                                                                                                   \n";
    
    
    protected int extraSpace;
    
    
    protected OutputStreamWriter writer;
    
    
    protected String about;

    
    public static final String XPACKET_PI_BEGIN = "<?xpacket begin=\"\u\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n";
    
    
    public static final String XPACKET_PI_END_W = "<?xpacket end=\"w\"?>";
    
    
    public static final String XPACKET_PI_END_R = "<?xpacket end=\"r\"?>";
    
    
    protected char end = 'w';
    
    
    public XmpWriter(OutputStream os, String utfEncoding, int extraSpace) throws IOException {
        this.extraSpace = extraSpace;
        writer = new OutputStreamWriter(os, utfEncoding);
        writer.write(XPACKET_PI_BEGIN);
        writer.write("<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">\n");
        writer.write("<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n");
        about = "";
    }
    
    
    public XmpWriter(OutputStream os) throws IOException {
        this(os, UTF8, 20);
    }
    
    
    public void setReadOnly() {
        end = 'r';
    }
    
    
    public void setAbout(String about) {
        this.about = about;
    }
    
    
    public void addRdfDescription(String xmlns, String content) throws IOException {
        writer.write("<rdf:Description rdf:about=\"");
        writer.write(about);
        writer.write("\" ");
        writer.write(xmlns);
        writer.write(">");
        writer.write(content);
        writer.write("</rdf:Description>\n");
    }
    
    
    public void addRdfDescription(XmpSchema s) throws IOException {
        writer.write("<rdf:Description rdf:about=\"");
        writer.write(about);
        writer.write("\" ");
        writer.write(s.getXmlns());
        writer.write(">");
        writer.write(s.toString());
        writer.write("</rdf:Description>\n");
    }
    
    
    public void close() throws IOException {
        writer.write("</rdf:RDF>");
        writer.write("</x:xmpmeta>\n");
        for (int i = 0; i < extraSpace; i++) {
            writer.write(EXTRASPACE);
        }
        writer.write(end == 'r' ? XPACKET_PI_END_R : XPACKET_PI_END_W);
        writer.flush();
        writer.close();
    }
    
    
    public XmpWriter(OutputStream os, PdfDictionary info, int PdfXConformance) throws IOException {
        this(os);
        if (info != null) {
            DublinCoreSchema dc = new DublinCoreSchema();
            PdfSchema p = new PdfSchema();
            XmpBasicSchema basic = new XmpBasicSchema();
            PdfName key;
            PdfObject obj;
            for (Iterator it = info.getKeys().iterator(); it.hasNext();) {
                key = (PdfName)it.next();
                obj = info.get(key);
                if (obj == null)
                    continue;
                if (PdfName.TITLE.equals(key)) {
                    dc.addTitle(((PdfString)obj).toUnicodeString());
                }
                if (PdfName.AUTHOR.equals(key)) {
                    dc.addAuthor(((PdfString)obj).toUnicodeString());
                }
                if (PdfName.SUBJECT.equals(key)) {
                    dc.addSubject(((PdfString)obj).toUnicodeString());
                    dc.addDescription(((PdfString)obj).toUnicodeString());
                }
                if (PdfName.KEYWORDS.equals(key)) {
                    p.addKeywords(((PdfString)obj).toUnicodeString());
                }
                if (PdfName.CREATOR.equals(key)) {
                    basic.addCreatorTool(((PdfString)obj).toUnicodeString());
                }
                if (PdfName.PRODUCER.equals(key)) {
                    p.addProducer(((PdfString)obj).toUnicodeString());
                }
                if (PdfName.CREATIONDATE.equals(key)) {
                    basic.addCreateDate(((PdfDate)obj).getW3CDate());
                }
                if (PdfName.MODDATE.equals(key)) {
                    basic.addModDate(((PdfDate)obj).getW3CDate());
                }
            }
            if (dc.size() > 0) addRdfDescription(dc);
            if (p.size() > 0) addRdfDescription(p);
            if (basic.size() > 0) addRdfDescription(basic);
            if (PdfXConformance == PdfWriter.PDFA1A || PdfXConformance == PdfWriter.PDFA1B) {
                PdfA1Schema a1 = new PdfA1Schema();
                if (PdfXConformance == PdfWriter.PDFA1A)
                    a1.addConformance("A");
                else
                    a1.addConformance("B");
                addRdfDescription(a1);
            }
        }
    }
    
    
    public XmpWriter(OutputStream os, Map info) throws IOException {
        this(os);
        if (info != null) {
            DublinCoreSchema dc = new DublinCoreSchema();
            PdfSchema p = new PdfSchema();
            XmpBasicSchema basic = new XmpBasicSchema();
            String key;
            String value;
            for (Iterator it = info.entrySet().iterator(); it.hasNext();) {
                Map.Entry entry = (Map.Entry) it.next();
                key = (String) entry.getKey();
                value = (String) entry.getValue();
                if (value == null)
                    continue;
                if ("Title".equals(key)) {
                    dc.addTitle(value);
                }
                if ("Author".equals(key)) {
                    dc.addAuthor(value);
                }
                if ("Subject".equals(key)) {
                    dc.addSubject(value);
                    dc.addDescription(value);
                }
                if ("Keywords".equals(key)) {
                    p.addKeywords(value);
                }
                if ("Creator".equals(key)) {
                    basic.addCreatorTool(value);
                }
                if ("Producer".equals(key)) {
                    p.addProducer(value);
                }
                if ("CreationDate".equals(key)) {
                    basic.addCreateDate(PdfDate.getW3CDate(value));
                }
                if ("ModDate".equals(key)) {
                    basic.addModDate(PdfDate.getW3CDate(value));
                }
            }
            if (dc.size() > 0) addRdfDescription(dc);
            if (p.size() > 0) addRdfDescription(p);
            if (basic.size() > 0) addRdfDescription(basic);
        }
    }
}