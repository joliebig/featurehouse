

package com.lowagie.text.pdf;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EmptyStackException;
import java.util.HashMap;
import java.util.Iterator;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.lowagie.text.xml.XmlDomWriter;


public class XfaForm {

    private Xml2SomTemplate templateSom;
    private Xml2SomDatasets datasetsSom;
    private AcroFieldsSearch acroFieldsSom;
    private PdfReader reader;
    private boolean xfaPresent;
    private org.w3c.dom.Document domDocument;
    private boolean changed;
    private Node datasetsNode;
    public static final String XFA_DATA_SCHEMA = "http://www.xfa.org/schema/xfa-data/1.0/";
    
    
    public XfaForm() {
    }
    
    
    public XfaForm(PdfReader reader) throws IOException, ParserConfigurationException, SAXException {
        this.reader = reader;
        PdfDictionary af = (PdfDictionary)PdfReader.getPdfObjectRelease(reader.getCatalog().get(PdfName.ACROFORM));
        if (af == null) {
            xfaPresent = false;
            return;
        }
        PdfObject xfa = PdfReader.getPdfObjectRelease(af.get(PdfName.XFA));
        if (xfa == null) {
            xfaPresent = false;
            return;
        }
        xfaPresent = true;
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        if (xfa.isArray()) {
            ArrayList ar = ((PdfArray)xfa).getArrayList();
            for (int k = 1; k < ar.size(); k += 2) {
                PdfObject ob = PdfReader.getPdfObject((PdfObject)ar.get(k));
                if (ob instanceof PRStream) {
                    byte[] b = PdfReader.getStreamBytes((PRStream)ob);
                    bout.write(b);
                }
            }
        }
        else if (xfa instanceof PRStream) {
            byte[] b = PdfReader.getStreamBytes((PRStream)xfa);
            bout.write(b);
        }
        bout.close();
        DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();
        fact.setNamespaceAware(true);
        DocumentBuilder db = fact.newDocumentBuilder();
        domDocument = db.parse(new ByteArrayInputStream(bout.toByteArray()));
        Node n = domDocument.getFirstChild();
        n = n.getFirstChild();
        while (n != null) {
            if (n.getNodeType() == Node.ELEMENT_NODE) {
                String s = n.getLocalName();
                if (s.equals("template")) {
                    templateSom = new Xml2SomTemplate(n);
                }
                else if (s.equals("datasets")) {
                    datasetsNode = n;
                    datasetsSom = new Xml2SomDatasets(n.getFirstChild());
                }
            }
            n = n.getNextSibling();
        }
    }
    
    
    public static void setXfa(byte[] xfaData, PdfReader reader, PdfWriter writer) throws IOException {
        PdfDictionary af = (PdfDictionary)PdfReader.getPdfObjectRelease(reader.getCatalog().get(PdfName.ACROFORM));
        if (af == null) {
            return;
        }
        reader.killXref(af.get(PdfName.XFA));
        PdfStream str = new PdfStream(xfaData);
        str.flateCompress();
        PdfIndirectReference ref = writer.addToBody(str).getIndirectReference();
        af.put(PdfName.XFA, ref);
    }

    
    public void setXfa(PdfWriter writer) throws IOException {
        setXfa(serializeDoc(domDocument), reader, writer);
    }

    
    public static byte[] serializeDoc(Node n) throws IOException {
        XmlDomWriter xw = new XmlDomWriter();
        ByteArrayOutputStream fout = new ByteArrayOutputStream();
        xw.setOutput(fout, null);
        xw.setCanonical(false);
        xw.write(n);
        fout.close();
        return fout.toByteArray();
    }
    
    
    public boolean isXfaPresent() {
        return xfaPresent;
    }

    
    public org.w3c.dom.Document getDomDocument() {
        return domDocument;
    }
    
    
    
    public String findFieldName(String name, AcroFields af) {
        HashMap items = af.getFields();
        if (items.containsKey(name))
            return name;
        if (acroFieldsSom == null) {
            acroFieldsSom = new AcroFieldsSearch(items.keySet());
        }
        if (acroFieldsSom.getAcroShort2LongName().containsKey(name))
            return (String)acroFieldsSom.getAcroShort2LongName().get(name);
        return acroFieldsSom.inverseSearchGlobal(Xml2Som.splitParts(name));
    }
    
    
    public String findDatasetsName(String name) {
        if (datasetsSom.getName2Node().containsKey(name))
            return name;
        return datasetsSom.inverseSearchGlobal(Xml2Som.splitParts(name));
    }

    
    public Node findDatasetsNode(String name) {
        if (name == null)
            return null;
        name = findDatasetsName(name);
        if (name == null)
            return null;
        return (Node)datasetsSom.getName2Node().get(name);
    }

    
    public static String getNodeText(Node n) {
        if (n == null)
            return "";
        return getNodeText(n, "");
        
    }
    
    private static String getNodeText(Node n, String name) {
        Node n2 = n.getFirstChild();
        while (n2 != null) {
            if (n2.getNodeType() == Node.ELEMENT_NODE) {
                name = getNodeText(n2, name);
            }
            else if (n2.getNodeType() == Node.TEXT_NODE) {
                name += n2.getNodeValue();
            }
            n2 = n2.getNextSibling();
        }
        return name;
    }
    
    
    public void setNodeText(Node n, String text) {
        if (n == null)
            return;
        Node nc = null;
        while ((nc = n.getFirstChild()) != null) {
            n.removeChild(nc);
        }
        if (n.getAttributes().getNamedItemNS(XFA_DATA_SCHEMA, "dataNode") != null)
            n.getAttributes().removeNamedItemNS(XFA_DATA_SCHEMA, "dataNode");
        n.appendChild(domDocument.createTextNode(text));
        changed = true;
    }
    
    
    public void setXfaPresent(boolean xfaPresent) {
        this.xfaPresent = xfaPresent;
    }

    
    public void setDomDocument(org.w3c.dom.Document domDocument) {
        this.domDocument = domDocument;
    }

    
    public PdfReader getReader() {
        return reader;
    }

    
    public void setReader(PdfReader reader) {
        this.reader = reader;
    }

    
    public boolean isChanged() {
        return changed;
    }

    
    public void setChanged(boolean changed) {
        this.changed = changed;
    }
    
    
    public static class InverseStore {
        protected ArrayList part = new ArrayList();
        protected ArrayList follow = new ArrayList();
        
        
        public String getDefaultName() {
            InverseStore store = this;
            while (true) {
                Object obj = store.follow.get(0);
                if (obj instanceof String)
                    return (String)obj;
                store = (InverseStore)obj;
            }
        }
        
        
        public boolean isSimilar(String name) {
            int idx = name.indexOf('[');
            name = name.substring(0, idx + 1);
            for (int k = 0; k < part.size(); ++k) {
                if (((String)part.get(k)).startsWith(name))
                    return true;
            }
            return false;
        }
    }

    
    public static class Stack2 extends ArrayList {
        private static final long serialVersionUID = -7451476576174095212L;

        
        public Object peek() {
            if (size() == 0)
                throw new EmptyStackException();
            return get(size() - 1);
        }
        
        
        public Object pop() {
            if (size() == 0)
                throw new EmptyStackException();
            Object ret = get(size() - 1);
            remove(size() - 1);
            return ret;
        }
        
        
        public Object push(Object item) {
            add(item);
            return item;
        }
        
        
        public boolean empty() {
            return size() == 0;
        }
    }
    
    
    public static class Xml2Som {
        
        protected ArrayList order;
        
        protected HashMap name2Node;
        
        protected HashMap inverseSearch;
        
        protected Stack2 stack;
        
        protected int anform;

        
        public static String escapeSom(String s) {
            int idx = s.indexOf('.');
            if (idx < 0)
                return s;
            StringBuffer sb = new StringBuffer();
            int last = 0;
            while (idx >= 0) {
                sb.append(s.substring(last, idx));
                sb.append('\\');
                last = idx;
                idx = s.indexOf('.', idx + 1);
            }
            sb.append(s.substring(last));
            return sb.toString();
        }

        
        public static String unescapeSom(String s) {
            int idx = s.indexOf('\\');
            if (idx < 0)
                return s;
            StringBuffer sb = new StringBuffer();
            int last = 0;
            while (idx >= 0) {
                sb.append(s.substring(last, idx));
                last = idx + 1;
                idx = s.indexOf('\\', idx + 1);
            }
            sb.append(s.substring(last));
            return sb.toString();
        }

        
        protected String printStack() {
            if (stack.empty())
                return "";
            StringBuffer s = new StringBuffer();
            for (int k = 0; k < stack.size(); ++k)
                s.append('.').append((String)stack.get(k));
            return s.substring(1);
        }
        
        
        public static String getShortName(String s) {
            int idx = s.indexOf(".#subform[");
            if (idx < 0)
                return s;
            int last = 0;
            StringBuffer sb = new StringBuffer();
            while (idx >= 0) {
                sb.append(s.substring(last, idx));
                idx = s.indexOf("]", idx + 10);
                if (idx < 0)
                    return sb.toString();
                last = idx + 1;
                idx = s.indexOf(".#subform[", last);
            }
            sb.append(s.substring(last));
            return sb.toString();
        }
        
        
        public void inverseSearchAdd(String unstack) {
            inverseSearchAdd(inverseSearch, stack, unstack);
        }
        
        
        public static void inverseSearchAdd(HashMap inverseSearch, Stack2 stack, String unstack) {
            String last = (String)stack.peek();
            InverseStore store = (InverseStore)inverseSearch.get(last);
            if (store == null) {
                store = new InverseStore();
                inverseSearch.put(last, store);
            }
            for (int k = stack.size() - 2; k >= 0; --k) {
                last = (String)stack.get(k);
                InverseStore store2;
                int idx = store.part.indexOf(last);
                if (idx < 0) {
                    store.part.add(last);
                    store2 = new InverseStore();
                    store.follow.add(store2);
                }
                else
                    store2 = (InverseStore)store.follow.get(idx);
                store = store2;
            }
            store.part.add("");
            store.follow.add(unstack);
        }

        
        public String inverseSearchGlobal(ArrayList parts) {
            if (parts.isEmpty())
                return null;
            InverseStore store = (InverseStore)inverseSearch.get(parts.get(parts.size() - 1));
            if (store == null)
                return null;
            for (int k = parts.size() - 2; k >= 0; --k) {
                String part = (String)parts.get(k);
                int idx = store.part.indexOf(part);
                if (idx < 0) {
                    if (store.isSimilar(part))
                        return null;
                    return store.getDefaultName();
                }
                store = (InverseStore)store.follow.get(idx);
            }
            return store.getDefaultName();
        }
    
        
        public static Stack2 splitParts(String name) {
            while (name.startsWith("."))
                name = name.substring(1);
            Stack2 parts = new Stack2();
            int last = 0;
            int pos = 0;
            String part;
            while (true) {
                pos = last;
                while (true) {
                    pos = name.indexOf('.', pos);
                    if (pos < 0)
                        break;
                    if (name.charAt(pos - 1) == '\\')
                        ++pos;
                    else
                        break;
                }
                if (pos < 0)
                    break;
                part = name.substring(last, pos);
                if (!part.endsWith("]"))
                    part += "[0]";
                parts.add(part);
                last = pos + 1;
            }
            part = name.substring(last);
            if (!part.endsWith("]"))
                part += "[0]";
            parts.add(part);
            return parts;
        }

        
        public ArrayList getOrder() {
            return order;
        }

        
        public void setOrder(ArrayList order) {
            this.order = order;
        }

        
        public HashMap getName2Node() {
            return name2Node;
        }

        
        public void setName2Node(HashMap name2Node) {
            this.name2Node = name2Node;
        }

        
        public HashMap getInverseSearch() {
            return inverseSearch;
        }

        
        public void setInverseSearch(HashMap inverseSearch) {
            this.inverseSearch = inverseSearch;
        }
    }
    
    
    public static class Xml2SomDatasets extends Xml2Som {
        
        public Xml2SomDatasets(Node n) {
            order = new ArrayList();
            name2Node = new HashMap();
            stack = new Stack2();
            anform = 0;
            inverseSearch = new HashMap();
            processDatasetsInternal(n);
        }

        
        public Node insertNode(Node n, String shortName) {
            Stack2 stack = splitParts(shortName);
            org.w3c.dom.Document doc = n.getOwnerDocument();
            Node n2 = null;
            n = n.getFirstChild();
            for (int k = 0; k < stack.size(); ++k) {
                String part = (String)stack.get(k);
                int idx = part.lastIndexOf('[');
                String name = part.substring(0, idx);
                idx = Integer.parseInt(part.substring(idx + 1, part.length() - 1));
                int found = -1;
                for (n2 = n.getFirstChild(); n2 != null; n2 = n2.getNextSibling()) {
                    if (n2.getNodeType() == Node.ELEMENT_NODE) {
                        String s = escapeSom(n2.getLocalName());
                        if (s.equals(name)) {
                            ++found;
                            if (found == idx)
                                break;
                        }
                    }
                }
                for (; found < idx; ++found) {
                    n2 = doc.createElementNS(null, name);
                    n2 = n.appendChild(n2);
                    Node attr = doc.createAttributeNS(XFA_DATA_SCHEMA, "dataNode");
                    attr.setNodeValue("dataGroup");
                    n2.getAttributes().setNamedItemNS(attr);
                }
                n = n2;
            }
            inverseSearchAdd(inverseSearch, stack, shortName);
            name2Node.put(shortName, n2);
            order.add(shortName);
            return n2;
        }
        
        private static boolean hasChildren(Node n) {
            Node dataNodeN = n.getAttributes().getNamedItemNS(XFA_DATA_SCHEMA, "dataNode");
            if (dataNodeN != null) {
                String dataNode = dataNodeN.getNodeValue();
                if ("dataGroup".equals(dataNode))
                    return true;
                else if ("dataValue".equals(dataNode))
                    return false;
            }
            if (!n.hasChildNodes())
                return false;
            Node n2 = n.getFirstChild();
            while (n2 != null) {
                if (n2.getNodeType() == Node.ELEMENT_NODE) {
                    return true;
                }
                n2 = n2.getNextSibling();
            }
            return false;
        }

        private void processDatasetsInternal(Node n) {
            HashMap ss = new HashMap();
            Node n2 = n.getFirstChild();
            while (n2 != null) {
                if (n2.getNodeType() == Node.ELEMENT_NODE) {
                    String s = escapeSom(n2.getLocalName());
                    Integer i = (Integer)ss.get(s);
                    if (i == null)
                        i = new Integer(0);
                    else
                        i = new Integer(i.intValue() + 1);
                    ss.put(s, i);
                    if (hasChildren(n2)) {
                        stack.push(s + "[" + i.toString() + "]");
                        processDatasetsInternal(n2);
                        stack.pop();
                    }
                    else {
                        stack.push(s + "[" + i.toString() + "]");
                        String unstack = printStack();
                        order.add(unstack);
                        inverseSearchAdd(unstack);
                        name2Node.put(unstack, n2);
                        stack.pop();
                    }
                }
                n2 = n2.getNextSibling();
            }
        }
    }

    
    public static class AcroFieldsSearch extends Xml2Som {
        private HashMap acroShort2LongName;
        
        
        public AcroFieldsSearch(Collection items) {
            inverseSearch = new HashMap();
            acroShort2LongName = new HashMap();
            for (Iterator it = items.iterator(); it.hasNext();) {
                String itemName = (String)it.next();
                String itemShort = getShortName(itemName);
                acroShort2LongName.put(itemShort, itemName);
                inverseSearchAdd(inverseSearch, splitParts(itemShort), itemName);
            }
        }

        
        public HashMap getAcroShort2LongName() {
            return acroShort2LongName;
        }

        
        public void setAcroShort2LongName(HashMap acroShort2LongName) {
            this.acroShort2LongName = acroShort2LongName;
        }
    }

    
    public static class Xml2SomTemplate extends Xml2Som {
        private boolean dynamicForm;
        private int templateLevel;
        
        
        public Xml2SomTemplate(Node n) {
            order = new ArrayList();
            name2Node = new HashMap();
            stack = new Stack2();
            anform = 0;
            templateLevel = 0;
            inverseSearch = new HashMap();
            processTemplate(n, null);
        }

        
        public String getFieldType(String s) {
            Node n = (Node)name2Node.get(s);
            if (n == null)
                return null;
            if (n.getLocalName().equals("exclGroup"))
                return "exclGroup";
            Node ui = n.getFirstChild();
            while (ui != null) {
                if (ui.getNodeType() == Node.ELEMENT_NODE && ui.getLocalName().equals("ui")) {
                    break;
                }
                ui = ui.getNextSibling();
            }
            if (ui == null)
                return null;
            Node type = ui.getFirstChild();
            while (type != null) {
                if (type.getNodeType() == Node.ELEMENT_NODE && !(type.getLocalName().equals("extras") && type.getLocalName().equals("picture"))) {
                    return type.getLocalName();
                }
                type = type.getNextSibling();
            }
            return null;
        }

        private void processTemplate(Node n, HashMap ff) {
            if (ff == null)
                ff = new HashMap();
            HashMap ss = new HashMap();
            Node n2 = n.getFirstChild();
            while (n2 != null) {
                if (n2.getNodeType() == Node.ELEMENT_NODE) {
                    String s = n2.getLocalName();
                    if (s.equals("subform")) {
                        Node name = n2.getAttributes().getNamedItem("name");
                        String nn = "#subform";
                        boolean annon = true;
                        if (name != null) {
                            nn = escapeSom(name.getNodeValue());
                            annon = false;
                        }
                        Integer i;
                        if (annon) {
                            i = new Integer(anform);
                            ++anform;
                        }
                        else {
                            i = (Integer)ss.get(nn);
                            if (i == null)
                                i = new Integer(0);
                            else
                                i = new Integer(i.intValue() + 1);
                            ss.put(nn, i);
                        }
                        stack.push(nn + "[" + i.toString() + "]");
                        ++templateLevel;
                        if (annon)
                            processTemplate(n2, ff);
                        else
                            processTemplate(n2, null);
                        --templateLevel;
                        stack.pop();
                    }
                    else if (s.equals("field") || s.equals("exclGroup")) {
                        Node name = n2.getAttributes().getNamedItem("name");
                        if (name != null) {
                            String nn = escapeSom(name.getNodeValue());
                            Integer i = (Integer)ff.get(nn);
                            if (i == null)
                                i = new Integer(0);
                            else
                                i = new Integer(i.intValue() + 1);
                            ff.put(nn, i);
                            stack.push(nn + "[" + i.toString() + "]");
                            String unstack = printStack();
                            order.add(unstack);
                            inverseSearchAdd(unstack);
                            name2Node.put(unstack, n2);
                            stack.pop();
                        }
                    }
                    else if (!dynamicForm && templateLevel > 0 && s.equals("occur")) {
                        int initial = 1;
                        int min = 1;
                        int max = 1;
                        Node a = n2.getAttributes().getNamedItem("initial");
                        if (a != null)
                            try{initial = Integer.parseInt(a.getNodeValue().trim());}catch(Exception e){}
                        a = n2.getAttributes().getNamedItem("min");
                        if (a != null)
                            try{min = Integer.parseInt(a.getNodeValue().trim());}catch(Exception e){}
                        a = n2.getAttributes().getNamedItem("max");
                        if (a != null)
                            try{max = Integer.parseInt(a.getNodeValue().trim());}catch(Exception e){}
                        if (initial != min || min != max)
                            dynamicForm = true;
                    }
                }
                n2 = n2.getNextSibling();
            }
        }

        
        public boolean isDynamicForm() {
            return dynamicForm;
        }

        
        public void setDynamicForm(boolean dynamicForm) {
            this.dynamicForm = dynamicForm;
        }
    }

    
    public Xml2SomTemplate getTemplateSom() {
        return templateSom;
    }

    
    public void setTemplateSom(Xml2SomTemplate templateSom) {
        this.templateSom = templateSom;
    }

    
    public Xml2SomDatasets getDatasetsSom() {
        return datasetsSom;
    }

    
    public void setDatasetsSom(Xml2SomDatasets datasetsSom) {
        this.datasetsSom = datasetsSom;
    }

    
    public AcroFieldsSearch getAcroFieldsSom() {
        return acroFieldsSom;
    }

    
    public void setAcroFieldsSom(AcroFieldsSearch acroFieldsSom) {
        this.acroFieldsSom = acroFieldsSom;
    }
    
    
    public Node getDatasetsNode() {
        return datasetsNode;
    }
}
