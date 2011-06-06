
package com.lowagie.text.pdf.events;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfPageEventHelper;
import com.lowagie.text.pdf.PdfWriter;


public class IndexEvents extends PdfPageEventHelper {

    
    private Map indextag = new TreeMap();

    
    public void onGenericTag(PdfWriter writer, Document document,
            Rectangle rect, String text) {
        indextag.put(text, new Integer(writer.getPageNumber()));
    }

    
    
    private long indexcounter = 0;

    
    private List indexentry = new ArrayList();

    
    public Chunk create(final String text, final String in1, final String in2,
            final String in3) {

        Chunk chunk = new Chunk(text);
        String tag = "idx_" + (indexcounter++);
        chunk.setGenericTag(tag);
        chunk.setLocalDestination(tag);
        Entry entry = new Entry(in1, in2, in3, tag);
        indexentry.add(entry);
        return chunk;
    }

    
    public Chunk create(final String text, final String in1) {
        return create(text, in1, "", "");
    }

    
    public Chunk create(final String text, final String in1, final String in2) {
        return create(text, in1, in2, "");
    }

    
    public void create(final Chunk text, final String in1, final String in2,
            final String in3) {

        String tag = "idx_" + (indexcounter++);
        text.setGenericTag(tag);
        text.setLocalDestination(tag);
        Entry entry = new Entry(in1, in2, in3, tag);
        indexentry.add(entry);
    }

    
    public void create(final Chunk text, final String in1) {
        create(text, in1, "", "");
    }

    
    public void create(final Chunk text, final String in1, final String in2) {
        create(text, in1, in2, "");
    }

    
    private Comparator comparator = new Comparator() {

        public int compare(Object arg0, Object arg1) {
            Entry en1 = (Entry) arg0;
            Entry en2 = (Entry) arg1;

            int rt = 0;
            if (en1.getIn1() != null && en2.getIn1() != null) {
                if ((rt = en1.getIn1().compareToIgnoreCase(en2.getIn1())) == 0) {
                    
                    if (en1.getIn2() != null && en2.getIn2() != null) {
                        if ((rt = en1.getIn2()
                                .compareToIgnoreCase(en2.getIn2())) == 0) {
                            
                            if (en1.getIn3() != null && en2.getIn3() != null) {
                                rt = en1.getIn3().compareToIgnoreCase(
                                        en2.getIn3());
                            }
                        }
                    }
                }
            }
            return rt;
        }
    };

    
    public void setComparator(Comparator aComparator) {
        comparator = aComparator;
    }

    
    public List getSortedEntries() {

        Map grouped = new HashMap();

        for (int i = 0; i < indexentry.size(); i++) {
            Entry e = (Entry) indexentry.get(i);
            String key = e.getKey();

            Entry master = (Entry) grouped.get(key);
            if (master != null) {
                master.addPageNumberAndTag(e.getPageNumber(), e.getTag());
            } else {
                e.addPageNumberAndTag(e.getPageNumber(), e.getTag());
                grouped.put(key, e);
            }
        }

        
        List sorted = new ArrayList(grouped.values());
        Collections.sort(sorted, comparator);
        return sorted;
    }

    
    
    public class Entry {

        
        private String in1;

        
        private String in2;

        
        private String in3;

        
        private String tag;

        
        private List pagenumbers = new ArrayList();

        
        private List tags = new ArrayList();

        
        public Entry(final String aIn1, final String aIn2, final String aIn3,
                final String aTag) {
            in1 = aIn1;
            in2 = aIn2;
            in3 = aIn3;
            tag = aTag;
        }

        
        public String getIn1() {
            return in1;
        }

        
        public String getIn2() {
            return in2;
        }

        
        public String getIn3() {
            return in3;
        }

        
        public String getTag() {
            return tag;
        }

        
        public int getPageNumber() {
            int rt = -1;
            Integer i = (Integer) indextag.get(tag);
            if (i != null) {
                rt = i.intValue();
            }
            return rt;
        }

        
        public void addPageNumberAndTag(final int number, final String tag) {
            pagenumbers.add(new Integer(number));
            tags.add(tag);
        }

        
        public String getKey() {
            return in1 + "!" + in2 + "!" + in3;
        }

        
        public List getPagenumbers() {
            return pagenumbers;
        }

        
        public List getTags() {
            return tags;
        }

        
        public String toString() {
            StringBuffer buf = new StringBuffer();
            buf.append(in1).append(' ');
            buf.append(in2).append(' ');
            buf.append(in3).append(' ');
            for (int i = 0; i < pagenumbers.size(); i++) {
                buf.append(pagenumbers.get(i)).append(' ');
            }
            return buf.toString();
        }
    }
}