

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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.StringTokenizer;

import com.lowagie.text.xml.simpleparser.IanaEncodings;
import com.lowagie.text.xml.simpleparser.SimpleXMLDocHandler;
import com.lowagie.text.xml.simpleparser.SimpleXMLParser;

public final class SimpleBookmark implements SimpleXMLDocHandler {
    
    private ArrayList topList;
    private Stack attr = new Stack();
    
    
    private SimpleBookmark() {
    }
    
    private static List bookmarkDepth(PdfReader reader, PdfDictionary outline, IntHashtable pages) {
        ArrayList list = new ArrayList();
        while (outline != null) {
            HashMap map = new HashMap();
            PdfString title = (PdfString)PdfReader.getPdfObjectRelease(outline.get(PdfName.TITLE));
            map.put("Title", title.toUnicodeString());
            PdfArray color = (PdfArray)PdfReader.getPdfObjectRelease(outline.get(PdfName.C));
            if (color != null && color.getArrayList().size() == 3) {
                ByteBuffer out = new ByteBuffer();
                ArrayList arr = color.getArrayList();
                out.append(((PdfNumber)arr.get(0)).floatValue()).append(' ');
                out.append(((PdfNumber)arr.get(1)).floatValue()).append(' ');
                out.append(((PdfNumber)arr.get(2)).floatValue());
                map.put("Color", PdfEncodings.convertToString(out.toByteArray(), null));
            }
            PdfNumber style = (PdfNumber)PdfReader.getPdfObjectRelease(outline.get(PdfName.F));
            if (style != null) {
                int f = style.intValue();
                String s = "";
                if ((f & 1) != 0)
                    s += "italic ";
                if ((f & 2) != 0)
                    s += "bold ";
                s = s.trim();
                if (s.length() != 0) 
                    map.put("Style", s);
            }
            PdfNumber count = (PdfNumber)PdfReader.getPdfObjectRelease(outline.get(PdfName.COUNT));
            if (count != null && count.intValue() < 0)
                map.put("Open", "false");
            try {
                PdfObject dest = PdfReader.getPdfObjectRelease(outline.get(PdfName.DEST));
                if (dest != null) {
                    mapGotoBookmark(map, dest, pages); 
                }
                else {
                    PdfDictionary action = (PdfDictionary)PdfReader.getPdfObjectRelease(outline.get(PdfName.A));
                    if (action != null) {
                        if (PdfName.GOTO.equals(PdfReader.getPdfObjectRelease(action.get(PdfName.S)))) {
                            dest = PdfReader.getPdfObjectRelease(action.get(PdfName.D));
                            if (dest != null) {
                                mapGotoBookmark(map, dest, pages);
                            }
                        }
                        else if (PdfName.URI.equals(PdfReader.getPdfObjectRelease(action.get(PdfName.S)))) {
                            map.put("Action", "URI");
                            map.put("URI", ((PdfString)PdfReader.getPdfObjectRelease(action.get(PdfName.URI))).toUnicodeString());
                        }
                        else if (PdfName.GOTOR.equals(PdfReader.getPdfObjectRelease(action.get(PdfName.S)))) {
                            dest = PdfReader.getPdfObjectRelease(action.get(PdfName.D));
                            if (dest != null) {
                                if (dest.isString())
                                    map.put("Named", dest.toString());
                                else if (dest.isName())
                                    map.put("NamedN", PdfName.decodeName(dest.toString()));
                                else if (dest.isArray()) {
                                    ArrayList arr = ((PdfArray)dest).getArrayList();
                                    StringBuffer s = new StringBuffer();
                                    s.append(arr.get(0).toString());
                                    s.append(' ').append(arr.get(1).toString());
                                    for (int k = 2; k < arr.size(); ++k)
                                        s.append(' ').append(arr.get(k).toString());
                                    map.put("Page", s.toString());
                                }
                            }
                            map.put("Action", "GoToR");
                            PdfObject file = PdfReader.getPdfObjectRelease(action.get(PdfName.F));
                            if (file != null) {
                                if (file.isString())
                                    map.put("File", ((PdfString)file).toUnicodeString());
                                else if (file.isDictionary()) {
                                    file = PdfReader.getPdfObject(((PdfDictionary)file).get(PdfName.F));
                                    if (file.isString())
                                        map.put("File", ((PdfString)file).toUnicodeString());
                                }
                            }
                            PdfObject newWindow = PdfReader.getPdfObjectRelease(action.get(PdfName.NEWWINDOW));
                            if (newWindow != null)
                                map.put("NewWindow", newWindow.toString());
                        }
                        else if (PdfName.LAUNCH.equals(PdfReader.getPdfObjectRelease(action.get(PdfName.S)))) {
                            map.put("Action", "Launch");
                            PdfObject file = PdfReader.getPdfObjectRelease(action.get(PdfName.F));
                            if (file == null)
                                file = PdfReader.getPdfObjectRelease(action.get(PdfName.WIN));
                            if (file != null) {
                                if (file.isString())
                                    map.put("File", ((PdfString)file).toUnicodeString());
                                else if (file.isDictionary()) {
                                    file = PdfReader.getPdfObjectRelease(((PdfDictionary)file).get(PdfName.F));
                                    if (file.isString())
                                        map.put("File", ((PdfString)file).toUnicodeString());
                                }
                            }
                        }
                    }
                }
            }
            catch (Exception e) {
                
            }
            PdfDictionary first = (PdfDictionary)PdfReader.getPdfObjectRelease(outline.get(PdfName.FIRST));
            if (first != null) {
                map.put("Kids", bookmarkDepth(reader, first, pages));
            }
            list.add(map);
            outline = (PdfDictionary)PdfReader.getPdfObjectRelease(outline.get(PdfName.NEXT));
        }
        return list;
    }
    
    private static void mapGotoBookmark(HashMap map, PdfObject dest, IntHashtable pages) 
    {
        if (dest.isString())
            map.put("Named", dest.toString());
        else if (dest.isName())
            map.put("Named", PdfName.decodeName(dest.toString()));
        else if (dest.isArray()) 
            map.put("Page", makeBookmarkParam((PdfArray)dest, pages)); 
        map.put("Action", "GoTo");
    }

    private static String makeBookmarkParam(PdfArray dest, IntHashtable pages)
    {
        ArrayList arr = dest.getArrayList();
        StringBuffer s = new StringBuffer();
        s.append(pages.get(getNumber((PdfIndirectReference)arr.get(0)))); 
        s.append(' ').append(arr.get(1).toString().substring(1));
        for (int k = 2; k < arr.size(); ++k)
            s.append(' ').append(arr.get(k).toString());
        return s.toString();
    }
    
    
    private static int getNumber(PdfIndirectReference indirect)
    {
        PdfDictionary pdfObj = (PdfDictionary)PdfReader.getPdfObjectRelease(indirect);
        if (pdfObj.contains(PdfName.TYPE) && pdfObj.get(PdfName.TYPE).equals(PdfName.PAGES) && pdfObj.contains(PdfName.KIDS)) 
        {
            PdfArray kids = (PdfArray)pdfObj.get(PdfName.KIDS);
            indirect = (PdfIndirectReference)kids.arrayList.get(0);
        }
        return indirect.getNumber();
    }
    
        
    public static List getBookmark(PdfReader reader) {
        PdfDictionary catalog = reader.getCatalog();
        PdfObject obj = PdfReader.getPdfObjectRelease(catalog.get(PdfName.OUTLINES));
        if (obj == null || !obj.isDictionary())
            return null;
        PdfDictionary outlines = (PdfDictionary)obj;
        IntHashtable pages = new IntHashtable();
        int numPages = reader.getNumberOfPages();
        for (int k = 1; k <= numPages; ++k) {
            pages.put(reader.getPageOrigRef(k).getNumber(), k);
            reader.releasePage(k);
        }
        return bookmarkDepth(reader, (PdfDictionary)PdfReader.getPdfObjectRelease(outlines.get(PdfName.FIRST)), pages);
    }
    
        
    public static void eliminatePages(List list, int pageRange[]) {
        if (list == null)
            return;
        for (Iterator it = list.listIterator(); it.hasNext();) {
            HashMap map = (HashMap)it.next();
            boolean hit = false;
            if ("GoTo".equals(map.get("Action"))) {
                String page = (String)map.get("Page");
                if (page != null) {
                    page = page.trim();
                    int idx = page.indexOf(' ');
                    int pageNum;
                    if (idx < 0)
                        pageNum = Integer.parseInt(page);
                    else
                        pageNum = Integer.parseInt(page.substring(0, idx));
                    int len = pageRange.length & 0xfffffffe;
                    for (int k = 0; k < len; k += 2) {
                        if (pageNum >= pageRange[k] && pageNum <= pageRange[k + 1]) {
                            hit = true;
                            break;
                        }
                    }
                }
            }
            List kids = (List)map.get("Kids");
            if (kids != null) {
                eliminatePages(kids, pageRange);
                if (kids.isEmpty()) {
                    map.remove("Kids");
                    kids = null;
                }
            }
            if (hit) {
                if (kids == null)
                    it.remove();
                else {
                    map.remove("Action");
                    map.remove("Page");
                    map.remove("Named");
                }
            }
        }
    }
    
        
    public static void shiftPageNumbers(List list, int pageShift, int pageRange[]) {
        if (list == null)
            return;
        for (Iterator it = list.listIterator(); it.hasNext();) {
            HashMap map = (HashMap)it.next();
            if ("GoTo".equals(map.get("Action"))) {
                String page = (String)map.get("Page");
                if (page != null) {
                    page = page.trim();
                    int idx = page.indexOf(' ');
                    int pageNum;
                    if (idx < 0)
                        pageNum = Integer.parseInt(page);
                    else
                        pageNum = Integer.parseInt(page.substring(0, idx));
                    boolean hit = false;
                    if (pageRange == null)
                        hit = true;
                    else {
                        int len = pageRange.length & 0xfffffffe;
                        for (int k = 0; k < len; k += 2) {
                            if (pageNum >= pageRange[k] && pageNum <= pageRange[k + 1]) {
                                hit = true;
                                break;
                            }
                        }
                    }
                    if (hit) {
                        if (idx < 0)
                            page = Integer.toString(pageNum + pageShift);
                        else
                            page = (pageNum + pageShift) + page.substring(idx);
                    }
                    map.put("Page", page);
                }
            }
            List kids = (List)map.get("Kids");
            if (kids != null)
                shiftPageNumbers(kids, pageShift, pageRange);
        }
    }
    
    static void createOutlineAction(PdfDictionary outline, HashMap map, PdfWriter writer, boolean namedAsNames) {
        try {
            String action = (String)map.get("Action");
            if ("GoTo".equals(action)) {
                String p;
                if ((p = (String)map.get("Named")) != null) {
                    if (namedAsNames)
                        outline.put(PdfName.DEST, new PdfName(p));
                    else
                        outline.put(PdfName.DEST, new PdfString(p, null));
                }
                else if ((p = (String)map.get("Page")) != null) {
                    PdfArray ar = new PdfArray();
                    StringTokenizer tk = new StringTokenizer(p);
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
                    outline.put(PdfName.DEST, ar);
                }
            }
            else if ("GoToR".equals(action)) {
                String p;
                PdfDictionary dic = new PdfDictionary();
                if ((p = (String)map.get("Named")) != null)
                    dic.put(PdfName.D, new PdfString(p, null));
                else if ((p = (String)map.get("NamedN")) != null)
                    dic.put(PdfName.D, new PdfName(p));
                else if ((p = (String)map.get("Page")) != null){
                    PdfArray ar = new PdfArray();
                    StringTokenizer tk = new StringTokenizer(p);
                    ar.add(new PdfNumber(tk.nextToken()));
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
                    dic.put(PdfName.D, ar);
                }
                String file = (String)map.get("File");
                if (dic.size() > 0 && file != null) {
                    dic.put(PdfName.S,  PdfName.GOTOR);
                    dic.put(PdfName.F, new PdfString(file));
                    String nw = (String)map.get("NewWindow");
                    if (nw != null) {
                        if (nw.equals("true"))
                            dic.put(PdfName.NEWWINDOW, PdfBoolean.PDFTRUE);
                        else if (nw.equals("false"))
                            dic.put(PdfName.NEWWINDOW, PdfBoolean.PDFFALSE);
                    }
                    outline.put(PdfName.A, dic);
                }
            }
            else if ("URI".equals(action)) {
                String uri = (String)map.get("URI");
                if (uri != null) {
                    PdfDictionary dic = new PdfDictionary();
                    dic.put(PdfName.S, PdfName.URI);
                    dic.put(PdfName.URI, new PdfString(uri));
                    outline.put(PdfName.A, dic);
                }
            }
            else if ("Launch".equals(action)) {
                String file = (String)map.get("File");
                if (file != null) {
                    PdfDictionary dic = new PdfDictionary();
                    dic.put(PdfName.S, PdfName.LAUNCH);
                    dic.put(PdfName.F, new PdfString(file));
                    outline.put(PdfName.A, dic);
                }
            }
        }
        catch (Exception e) {
            
        }
    }

    public static Object[] iterateOutlines(PdfWriter writer, PdfIndirectReference parent, List kids, boolean namedAsNames) throws IOException {
        PdfIndirectReference refs[] = new PdfIndirectReference[kids.size()];
        for (int k = 0; k < refs.length; ++k)
            refs[k] = writer.getPdfIndirectReference();
        int ptr = 0;
        int count = 0;
        for (Iterator it = kids.listIterator(); it.hasNext(); ++ptr) {
            HashMap map = (HashMap)it.next();
            Object lower[] = null;
            List subKid = (List)map.get("Kids");
            if (subKid != null && !subKid.isEmpty())
                lower = iterateOutlines(writer, refs[ptr], subKid, namedAsNames);
            PdfDictionary outline = new PdfDictionary();
            ++count;
            if (lower != null) {
                outline.put(PdfName.FIRST, (PdfIndirectReference)lower[0]);
                outline.put(PdfName.LAST, (PdfIndirectReference)lower[1]);
                int n = ((Integer)lower[2]).intValue();
                if ("false".equals(map.get("Open"))) {
                    outline.put(PdfName.COUNT, new PdfNumber(-n));
                }
                else {
                    outline.put(PdfName.COUNT, new PdfNumber(n));
                    count += n;
                }
            }
            outline.put(PdfName.PARENT, parent);
            if (ptr > 0)
                outline.put(PdfName.PREV, refs[ptr - 1]);
            if (ptr < refs.length - 1)
                outline.put(PdfName.NEXT, refs[ptr + 1]);
            outline.put(PdfName.TITLE, new PdfString((String)map.get("Title"), PdfObject.TEXT_UNICODE));
            String color = (String)map.get("Color");
            if (color != null) {
                try {
                    PdfArray arr = new PdfArray();
                    StringTokenizer tk = new StringTokenizer(color);
                    for (int k = 0; k < 3; ++k) {
                        float f = Float.parseFloat(tk.nextToken());
                        if (f < 0) f = 0;
                        if (f > 1) f = 1;
                        arr.add(new PdfNumber(f));
                    }
                    outline.put(PdfName.C, arr);
                } catch(Exception e){} 
            }
            String style = (String)map.get("Style");
            if (style != null) {
                style = style.toLowerCase();
                int bits = 0;
                if (style.indexOf("italic") >= 0)
                    bits |= 1;
                if (style.indexOf("bold") >= 0)
                    bits |= 2;
                if (bits != 0)
                    outline.put(PdfName.F, new PdfNumber(bits));
            }
            createOutlineAction(outline, map, writer, namedAsNames);
            writer.addToBody(outline, refs[ptr]);
        }
        return new Object[]{refs[0], refs[refs.length - 1], new Integer(count)};
    }
    
    
    public static void exportToXMLNode(List list, Writer out, int indent, boolean onlyASCII) throws IOException {
        String dep = "";
        for (int k = 0; k < indent; ++k)
            dep += "  ";
        for (Iterator it = list.iterator(); it.hasNext();) {
            HashMap map = (HashMap)it.next();
            String title = null;
            out.write(dep);
            out.write("<Title ");
            List kids = null;
            for (Iterator e = map.entrySet().iterator(); e.hasNext();) {
                Map.Entry entry = (Map.Entry) e.next();
                String key = (String) entry.getKey();
                if (key.equals("Title")) {
                    title = (String) entry.getValue();
                    continue;
                }
                else if (key.equals("Kids")) {
                    kids = (List) entry.getValue();
                    continue;
                }
                else {
                    out.write(key);
                    out.write("=\"");
                    String value = (String) entry.getValue();
                    if (key.equals("Named") || key.equals("NamedN"))
                        value = SimpleNamedDestination.escapeBinaryString(value);
                    out.write(SimpleXMLParser.escapeXML(value, onlyASCII));
                    out.write("\" ");
                }
            }
            out.write(">");
            if (title == null)
                title = "";
            out.write(SimpleXMLParser.escapeXML(title, onlyASCII));
            if (kids != null) {
                out.write("\n");
                exportToXMLNode(kids, out, indent + 1, onlyASCII);
                out.write(dep);
            }
            out.write("</Title>\n");
        }
    }
    
        
    public static void exportToXML(List list, OutputStream out, String encoding, boolean onlyASCII) throws IOException {
        String jenc = IanaEncodings.getJavaEncoding(encoding);
        Writer wrt = new BufferedWriter(new OutputStreamWriter(out, jenc));
        exportToXML(list, wrt, encoding, onlyASCII);
    }
    
    
    public static void exportToXML(List list, Writer wrt, String encoding, boolean onlyASCII) throws IOException {
        wrt.write("<?xml version=\"1.0\" encoding=\"");
        wrt.write(SimpleXMLParser.escapeXML(encoding, onlyASCII));
        wrt.write("\"?>\n<Bookmark>\n");
        exportToXMLNode(list, wrt, 1, onlyASCII);
        wrt.write("</Bookmark>\n");
        wrt.flush();
    }
    
        
    public static List importFromXML(InputStream in) throws IOException {
        SimpleBookmark book = new SimpleBookmark();
        SimpleXMLParser.parse(book, in);
        return book.topList;
    }
    
    
    public static List importFromXML(Reader in) throws IOException {
        SimpleBookmark book = new SimpleBookmark();
        SimpleXMLParser.parse(book, in);
        return book.topList;
    }
    
    public void endDocument() {
    }
    
    public void endElement(String tag) {
        if (tag.equals("Bookmark")) {
            if (attr.isEmpty())
                return;
            else
                throw new RuntimeException("Bookmark end tag out of place.");
        }
        if (!tag.equals("Title"))
            throw new RuntimeException("Invalid end tag - " + tag);
        HashMap attributes = (HashMap)attr.pop();
        String title = (String)attributes.get("Title");
        attributes.put("Title",  title.trim());
        String named = (String)attributes.get("Named");
        if (named != null)
            attributes.put("Named", SimpleNamedDestination.unEscapeBinaryString(named));
        named = (String)attributes.get("NamedN");
        if (named != null)
            attributes.put("NamedN", SimpleNamedDestination.unEscapeBinaryString(named));
        if (attr.isEmpty())
            topList.add(attributes);
        else {
            HashMap parent = (HashMap)attr.peek();
            List kids = (List)parent.get("Kids");
            if (kids == null) {
                kids = new ArrayList();
                parent.put("Kids", kids);
            }
            kids.add(attributes);
        }
    }
    
    public void startDocument() {
    }
    
    public void startElement(String tag, HashMap h) {
        if (topList == null) {
            if (tag.equals("Bookmark")) {
                topList = new ArrayList();
                return;
            }
            else
                throw new RuntimeException("Root element is not Bookmark: " + tag);
        }
        if (!tag.equals("Title"))
            throw new RuntimeException("Tag " + tag + " not allowed.");
        HashMap attributes = new HashMap(h);
        attributes.put("Title", "");
        attributes.remove("Kids");
        attr.push(attributes);
    }
    
    public void text(String str) {
        if (attr.isEmpty())
            return;
        HashMap attributes = (HashMap)attr.peek();
        String title = (String)attributes.get("Title");
        title += str;
        attributes.put("Title", title);
    }    
}
