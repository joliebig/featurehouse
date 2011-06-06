
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

    
    private Map<String, Integer> indextag = new TreeMap<String, Integer>();

    
    public void onGenericTag(PdfWriter writer, Document document,
            Rectangle rect, String text) {
        indextag.put(text, new Integer(writer.getPageNumber()));
    }

    
    
    private long indexcounter = 0;

    
    private List<Entry> indexentry = new ArrayList<Entry>();

    
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

    
    private Comparator<Entry> comparator = new Comparator<Entry>() {

        public int compare(Entry en1, Entry en2) {
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

    
    public void setComparator(Comparator<Entry> aComparator) {
        comparator = aComparator;
    }

    
    public List<Entry> getSortedEntries() {

        Map<String, Entry> grouped = new HashMap<String, Entry>();

        for (int i = 0; i < indexentry.size(); i++) {
            Entry e = indexentry.get(i);
            String key = e.getKey();

            Entry master = grouped.get(key);
            if (master != null) {
                master.addPageNumberAndTag(e.getPageNumber(), e.getTag());
            } else {
                e.addPageNumberAndTag(e.getPageNumber(), e.getTag());
                grouped.put(key, e);
            }
        }

        
        List<Entry> sorted = new ArrayList<Entry>(grouped.values());
        Collections.sort(sorted, comparator);
        return sorted;
    }

    
    
    public class Entry {

        
        private String in1;

        
        private String in2;

        
        private String in3;

        
        private String tag;

        
        private List<Integer> pagenumbers = new ArrayList<Integer>();

        
        private List<String> tags = new ArrayList<String>();

        
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
            Integer i = indextag.get(tag);
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

        
        public List<Integer> getPagenumbers() {
            return pagenumbers;
        }

        
        public List<String> getTags() {
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