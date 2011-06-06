
package com.lowagie.text.pdf;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import com.lowagie.text.xml.simpleparser.IanaEncodings;
import com.lowagie.text.xml.simpleparser.SimpleXMLDocHandler;
import com.lowagie.text.xml.simpleparser.SimpleXMLParser;


public final class SimpleNamedDestination implements SimpleXMLDocHandler {
    
    private HashMap<String, String> xmlNames;
    private HashMap<String, String> xmlLast;

    private SimpleNamedDestination() {
    }
    
    public static HashMap<String, String> getNamedDestination(PdfReader reader, boolean fromNames) {
        IntHashtable pages = new IntHashtable();
        int numPages = reader.getNumberOfPages();
        for (int k = 1; k <= numPages; ++k)
            pages.put(reader.getPageOrigRef(k).getNumber(), k);
        HashMap<String, PdfObject> names = fromNames ? reader.getNamedDestinationFromNames() : reader.getNamedDestinationFromStrings();
        HashMap<String, String> n2 = new HashMap<String, String>(names.size()); 
        for (Map.Entry<String, PdfObject> entry: names.entrySet()) {
            ArrayList<PdfObject> arr = ((PdfArray)entry.getValue()).getArrayList();
            StringBuffer s = new StringBuffer();
            try {
                s.append(pages.get(((PdfIndirectReference)arr.get(0)).getNumber()));
                s.append(' ').append(arr.get(1).toString().substring(1));
                for (int k = 2; k < arr.size(); ++k)
                    s.append(' ').append(arr.get(k).toString());
                n2.put(entry.getKey(), s.toString());
            }
            catch (Exception e) {
            }
        }
        return n2;
    }
    
    
    public static void exportToXML(HashMap<String, String> names, OutputStream out, String encoding, boolean onlyASCII) throws IOException {
        String jenc = IanaEncodings.getJavaEncoding(encoding);
        Writer wrt = new BufferedWriter(new OutputStreamWriter(out, jenc));
        exportToXML(names, wrt, encoding, onlyASCII);
    }
    
    
    public static void exportToXML(HashMap<String, String> names, Writer wrt, String encoding, boolean onlyASCII) throws IOException {
        wrt.write("<?xml version=\"1.0\" encoding=\"");
        wrt.write(SimpleXMLParser.escapeXML(encoding, onlyASCII));
        wrt.write("\"?>\n<Destination>\n");
        for (Map.Entry<String, String> entry: names.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue();
            wrt.write("  <Name Page=\"");
            wrt.write(SimpleXMLParser.escapeXML(value, onlyASCII));
            wrt.write("\">");
            wrt.write(SimpleXMLParser.escapeXML(escapeBinaryString(key), onlyASCII));
            wrt.write("</Name>\n");
        }
        wrt.write("</Destination>\n");
        wrt.flush();
    }
    
    
    public static HashMap<String, String> importFromXML(InputStream in) throws IOException {
        SimpleNamedDestination names = new SimpleNamedDestination();
        SimpleXMLParser.parse(names, in);
        return names.xmlNames;
    }
    
    
    public static HashMap<String, String> importFromXML(Reader in) throws IOException {
        SimpleNamedDestination names = new SimpleNamedDestination();
        SimpleXMLParser.parse(names, in);
        return names.xmlNames;
    }

    static PdfArray createDestinationArray(String value, PdfWriter writer) {
        PdfArray ar = new PdfArray();
        StringTokenizer tk = new StringTokenizer(value);
        int n = Integer.parseInt(tk.nextToken());
        ar.add(writer.getPageReference(n));
        if (!tk.hasMoreTokens()) {
            ar.add(PdfName.XYZ);
            ar.add(new float[]{0, 10000, 0});
        }
        else {
            String fn = tk.nextToken();
            if (fn.startsWith("/"))
                fn = fn.substring(1);
            ar.add(new PdfName(fn));
            for (int k = 0; k < 4 && tk.hasMoreTokens(); ++k) {
                fn = tk.nextToken();
                if (fn.equals("null"))
                    ar.add(PdfNull.PDFNULL);
                else
                    ar.add(new PdfNumber(fn));
            }
        }
        return ar;
    }
    
    public static PdfDictionary outputNamedDestinationAsNames(HashMap<String, String> names, PdfWriter writer) {
        PdfDictionary dic = new PdfDictionary();
        for (Map.Entry<String, String> entry: names.entrySet()) {
            try {
                String key = entry.getKey();
                String value = entry.getValue();
                PdfArray ar = createDestinationArray(value, writer);
                PdfName kn = new PdfName(key);
                dic.put(kn, ar);
            }
            catch (Exception e) {
                
            }            
        }
        return dic;
    }
    
    public static PdfDictionary outputNamedDestinationAsStrings(HashMap<String, String> names, PdfWriter writer) throws IOException {
        HashMap<String, PdfObject> n2 = new HashMap<String, PdfObject>(names.size());
        for (Map.Entry<String, String> entry: names.entrySet()) {
            try {
                String value = entry.getValue();
                PdfArray ar = createDestinationArray(value, writer);
                n2.put(entry.getKey(), writer.addToBody(ar).getIndirectReference());
            }
            catch (Exception e) {
            }
        }
        return PdfNameTree.writeTree(n2, writer);
    }
    
    public static String escapeBinaryString(String s) {
        StringBuffer buf = new StringBuffer();
        char cc[] = s.toCharArray();
        int len = cc.length;
        for (int k = 0; k < len; ++k) {
            char c = cc[k];
            if (c < ' ') {
                buf.append('\\');
                String octal = "00" + Integer.toOctalString(c);
                buf.append(octal.substring(octal.length() - 3));
            }
            else if (c == '\\')
                buf.append("\\\\");
            else
                buf.append(c);
        }
        return buf.toString();
    }
    
    public static String unEscapeBinaryString(String s) {
        StringBuffer buf = new StringBuffer();
        char cc[] = s.toCharArray();
        int len = cc.length;
        for (int k = 0; k < len; ++k) {
            char c = cc[k];
            if (c == '\\') {
                if (++k >= len) {
                    buf.append('\\');
                    break;
                }
                c = cc[k];
                if (c >= '0' && c <= '7') {
                    int n = c - '0';
                    ++k;
                    for (int j = 0; j < 2 && k < len; ++j) {
                        c = cc[k];
                        if (c >= '0' && c <= '7') {
                            ++k;
                            n = n * 8 + c - '0';
                        }
                        else {
                            break;
                        }
                    }
                    --k;
                    buf.append((char)n);
                }
                else
                    buf.append(c);
            }
            else
                buf.append(c);
        }
        return buf.toString();
    }
    
    public void endDocument() {
    }
    
    public void endElement(String tag) {
        if (tag.equals("Destination")) {
            if (xmlLast == null && xmlNames != null)
                return;
            else
                throw new RuntimeException("Destination end tag out of place.");
        }
        if (!tag.equals("Name"))
            throw new RuntimeException("Invalid end tag - " + tag);
        if (xmlLast == null || xmlNames == null)
            throw new RuntimeException("Name end tag out of place.");
        if (!xmlLast.containsKey("Page"))
            throw new RuntimeException("Page attribute missing.");
        xmlNames.put(unEscapeBinaryString(xmlLast.get("Name")), xmlLast.get("Page"));
        xmlLast = null;
    }
    
    public void startDocument() {
    }
    
    public void startElement(String tag, HashMap<String, String> h) {
        if (xmlNames == null) {
            if (tag.equals("Destination")) {
                xmlNames = new HashMap<String, String>();
                return;
            }
            else
                throw new RuntimeException("Root element is not Destination.");
        }
        if (!tag.equals("Name"))
            throw new RuntimeException("Tag " + tag + " not allowed.");
        if (xmlLast != null)
            throw new RuntimeException("Nested tags are not allowed.");
        xmlLast = new HashMap<String, String>(h);
        xmlLast.put("Name", "");
    }
    
    public void text(String str) {
        if (xmlLast == null)
            return;
        String name = xmlLast.get("Name");
        name += str;
        xmlLast.put("Name", name);
    }    
}