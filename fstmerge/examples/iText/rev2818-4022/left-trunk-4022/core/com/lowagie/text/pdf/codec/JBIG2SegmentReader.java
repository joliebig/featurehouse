

package com.lowagie.text.pdf.codec;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import com.lowagie.text.pdf.RandomAccessFileOrArray;



public class JBIG2SegmentReader {
    
    public static final int SYMBOL_DICTIONARY = 0; 

    public static final int INTERMEDIATE_TEXT_REGION = 4; 
    public static final int IMMEDIATE_TEXT_REGION = 6; 
    public static final int IMMEDIATE_LOSSLESS_TEXT_REGION = 7; 
    public static final int PATTERN_DICTIONARY = 16; 
    public static final int INTERMEDIATE_HALFTONE_REGION = 20; 
    public static final int IMMEDIATE_HALFTONE_REGION = 22; 
    public static final int IMMEDIATE_LOSSLESS_HALFTONE_REGION = 23; 
    public static final int INTERMEDIATE_GENERIC_REGION = 36; 
    public static final int IMMEDIATE_GENERIC_REGION = 38; 
    public static final int IMMEDIATE_LOSSLESS_GENERIC_REGION = 39; 
    public static final int INTERMEDIATE_GENERIC_REFINEMENT_REGION = 40; 
    public static final int IMMEDIATE_GENERIC_REFINEMENT_REGION = 42; 
    public static final int IMMEDIATE_LOSSLESS_GENERIC_REFINEMENT_REGION = 43; 

    public static final int PAGE_INFORMATION = 48; 
    public static final int END_OF_PAGE = 49; 
    public static final int END_OF_STRIPE = 50; 
    public static final int END_OF_FILE = 51; 
    public static final int PROFILES = 52; 
    public static final int TABLES = 53; 
    public static final int EXTENSION = 62; 
    
    private final SortedMap segments = new TreeMap();
    private final SortedMap pages = new TreeMap();
    private final SortedSet globals = new TreeSet();
    private RandomAccessFileOrArray ra;
    private boolean sequential;
    private boolean number_of_pages_known;
    private int number_of_pages = -1;
    private boolean read = false;
    
    
    public static class JBIG2Segment implements Comparable {

        public final int segmentNumber;
        public long dataLength = -1;
        public int page = -1;
        public int[] referredToSegmentNumbers = null;
        public boolean[] segmentRetentionFlags = null;
        public int type = -1;
        public boolean deferredNonRetain = false;
        public int countOfReferredToSegments = -1;
        public byte[] data = null;
        public byte[] headerData = null;
        public boolean page_association_size = false;
        public int page_association_offset = -1;

        public JBIG2Segment(int segment_number) {
            this.segmentNumber = segment_number;
        }

        
        public int compareTo(Object o) {
            return this.compareTo((JBIG2Segment)o);
        }
        public int compareTo(JBIG2Segment s) {
            return this.segmentNumber - s.segmentNumber;
        }

        
    }
    
    public static class JBIG2Page {
        public final int page;
        private final JBIG2SegmentReader sr;
        private final SortedMap segs = new TreeMap();
        public int pageBitmapWidth = -1;
        public int pageBitmapHeight = -1;
        public JBIG2Page(int page, JBIG2SegmentReader sr) {
            this.page = page;
            this.sr = sr;
        }
        
        public byte[] getData(boolean for_embedding) throws IOException {
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            for (Iterator i = segs.keySet().iterator(); i.hasNext();  ) {
                Integer sn = (Integer) i.next();
                JBIG2Segment s = (JBIG2Segment) segs.get(sn);

                
                
                if ( for_embedding && 
                        ( s.type == END_OF_FILE || s.type == END_OF_PAGE ) ) {
                    continue;
                }

                if ( for_embedding ) {
                    
                    byte[] headerData_emb = copyByteArray(s.headerData);
                    if ( s.page_association_size ) {
                        headerData_emb[s.page_association_offset] = 0x0;
                        headerData_emb[s.page_association_offset+1] = 0x0;
                        headerData_emb[s.page_association_offset+2] = 0x0;
                        headerData_emb[s.page_association_offset+3] = 0x1;
                    } else {
                        headerData_emb[s.page_association_offset] = 0x1;
                    }
                    os.write(headerData_emb);
                } else {
                    os.write(s.headerData);
                }
                os.write(s.data);
            }
            os.close();
            return os.toByteArray();
        }
        public void addSegment(JBIG2Segment s) {
            segs.put(new Integer(s.segmentNumber), s);
        }
        
    }
    
    public JBIG2SegmentReader(RandomAccessFileOrArray ra ) throws IOException {
        this.ra = ra;
    }

    public static byte[] copyByteArray(byte[] b) {
        byte[] bc = new byte[b.length];
        System.arraycopy(b, 0, bc, 0, b.length);
        return bc;
    }

    public void read() throws IOException {
        if ( this.read ) {
            throw new IllegalStateException("already attempted a read() on this Jbig2 File");
        }
        this.read = true;
        
        readFileHeader();
        
        if ( this.sequential ) {
            
            do {
                JBIG2Segment tmp = readHeader();
                readSegment(tmp);
                segments.put(new Integer(tmp.segmentNumber), tmp);
            } while ( this.ra.getFilePointer() < this.ra.length() );
        } else {
            
            JBIG2Segment tmp;
            do {
                tmp = readHeader();
                segments.put(new Integer(tmp.segmentNumber), tmp);
            } while ( tmp.type != END_OF_FILE );
            Iterator segs = segments.keySet().iterator();
            while ( segs.hasNext() ) {
                readSegment((JBIG2Segment)segments.get(segs.next()));
            }
        }
    }

    void readSegment(JBIG2Segment s) throws IOException {
        int ptr = ra.getFilePointer();
        
        if ( s.dataLength == 0xffffffffl ) {
            
            return;
        }
        
        byte[] data = new byte[(int)s.dataLength];
        ra.read(data);
        s.data = data;
        
        if ( s.type == PAGE_INFORMATION ) {
            int last = ra.getFilePointer();
            ra.seek(ptr);
            int page_bitmap_width = ra.readInt();
            int page_bitmap_height = ra.readInt();
            ra.seek(last);
            JBIG2Page p = (JBIG2Page)pages.get(new Integer(s.page));
            if ( p == null ) {
                throw new IllegalStateException("referring to widht/height of page we havent seen yet? " + s.page);
            }
            
            p.pageBitmapWidth = page_bitmap_width;
            p.pageBitmapHeight = page_bitmap_height;
        }
    }

    JBIG2Segment readHeader() throws IOException {
        int ptr = ra.getFilePointer();
        
        int segment_number = ra.readInt();
        JBIG2Segment s = new JBIG2Segment(segment_number);

        
        int segment_header_flags = ra.read();
        boolean deferred_non_retain = (( segment_header_flags & 0x80 ) == 0x80);
        s.deferredNonRetain = deferred_non_retain;
        boolean page_association_size = (( segment_header_flags & 0x40 ) == 0x40);
        int segment_type = ( segment_header_flags & 0x3f );
        s.type = segment_type;
        
        
        int referred_to_byte0 = ra.read();
        int count_of_referred_to_segments = (referred_to_byte0 & 0xE0) >> 5;
        int[] referred_to_segment_numbers = null;
        boolean[] segment_retention_flags = null;
        
        if ( count_of_referred_to_segments == 7 ) {
            
            ra.seek(ra.getFilePointer() - 1);
            count_of_referred_to_segments = ( ra.readInt() & 0x1fffffff );
            segment_retention_flags = new boolean[count_of_referred_to_segments+1];
            int i = 0;
            int referred_to_current_byte = 0;
            do {
                int j = i % 8;
                if ( j == 0) {
                    referred_to_current_byte = ra.read();
                }
                segment_retention_flags[i] = (((( 0x1 << j ) & referred_to_current_byte) >> j) == 0x1);
                i++;
            } while ( i <= count_of_referred_to_segments );
            
        } else if ( count_of_referred_to_segments <= 4 ) {
            
            segment_retention_flags = new boolean[count_of_referred_to_segments+1];
            referred_to_byte0 &= 0x1f;
            for ( int i = 0; i <= count_of_referred_to_segments; i++ ) {
                segment_retention_flags[i] = (((( 0x1 << i ) & referred_to_byte0) >> i) == 0x1); 
            }
            
        } else if ( count_of_referred_to_segments == 5 || count_of_referred_to_segments == 6 ) {
            throw new IllegalStateException("count of referred-to segments had bad value in header for segment " + segment_number + " starting at " + ptr);
        }
        s.segmentRetentionFlags = segment_retention_flags;
        s.countOfReferredToSegments = count_of_referred_to_segments;

        
        referred_to_segment_numbers = new int[count_of_referred_to_segments+1];
        for ( int i = 1; i <= count_of_referred_to_segments; i++ ) {
            if ( segment_number <= 256 ) {
                referred_to_segment_numbers[i] = ra.read();
            } else if ( segment_number <= 65536 ) {
                referred_to_segment_numbers[i] = ra.readUnsignedShort();
            } else {
                referred_to_segment_numbers[i] = (int)ra.readUnsignedInt(); 
            }
        }
        s.referredToSegmentNumbers = referred_to_segment_numbers;
        
        
        int segment_page_association;
        int page_association_offset = ra.getFilePointer() - ptr;
        if ( page_association_size ) {
            segment_page_association = ra.readInt();
        } else {
            segment_page_association = ra.read();
        }
        if ( segment_page_association < 0 ) {
            throw new IllegalStateException("page " + segment_page_association + " invalid for segment " + segment_number + " starting at " + ptr);
        }
        s.page = segment_page_association;
        
        s.page_association_size = page_association_size;
        s.page_association_offset = page_association_offset;
        
        if ( segment_page_association > 0 && ! pages.containsKey(new Integer(segment_page_association)) ) {
            pages.put(new Integer(segment_page_association), new JBIG2Page(segment_page_association, this));
        }
        if ( segment_page_association > 0 ) {
            ((JBIG2Page)pages.get(new Integer(segment_page_association))).addSegment(s);
        } else {
            globals.add(s);
        }
        
        
        long segment_data_length = ra.readUnsignedInt();
        
        s.dataLength = segment_data_length;
        
        int end_ptr = ra.getFilePointer();
        ra.seek(ptr);
        byte[] header_data = new byte[end_ptr - ptr];
        ra.read(header_data);
        s.headerData  = header_data;
        
        return s;
    }

    void readFileHeader() throws IOException {
        ra.seek(0);
        byte[] idstring = new byte[8];
        ra.read(idstring);
        
        byte[] refidstring = {(byte)0x97, 0x4A, 0x42, 0x32, 0x0D, 0x0A, 0x1A, 0x0A};
        
        for ( int i = 0; i < idstring.length; i++ ) {
            if ( idstring[i] != refidstring[i] ) {
                throw new IllegalStateException("file header idstring not good at byte " + i);
            }
        }
        
        int fileheaderflags = ra.read();

        this.sequential = (( fileheaderflags & 0x1 ) == 0x1);
        this.number_of_pages_known = (( fileheaderflags & 0x2) == 0x0);
        
        if ( (fileheaderflags & 0xfc) != 0x0 ) {
            throw new IllegalStateException("file header flags bits 2-7 not 0");
        }
        
        if ( this.number_of_pages_known ) {
            this.number_of_pages = ra.readInt();
        }
    }

    public int numberOfPages() {
        return pages.size();
    }

    public int getPageHeight(int i) {
        return ((JBIG2Page)pages.get(new Integer(i))).pageBitmapHeight;
    }

    public int getPageWidth(int i) {
        return ((JBIG2Page)pages.get(new Integer(i))).pageBitmapWidth;
    }

    public JBIG2Page getPage(int page) {
        return (JBIG2Page)pages.get(new Integer(page));
    }

    public byte[] getGlobal(boolean for_embedding) {
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        try {
            for (Iterator gitr = globals.iterator(); gitr.hasNext();) {
                JBIG2Segment s = (JBIG2Segment)gitr.next();
                if ( for_embedding && 
                        ( s.type == END_OF_FILE || s.type == END_OF_PAGE ) ) {
                    continue;
                }
                os.write(s.headerData);
                os.write(s.data);
            }
            os.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        if ( os.size() <= 0 ) {
            return null;
        }
        return os.toByteArray();
    }
    
    public String toString() {
        if ( this.read ) {
            return "Jbig2SegmentReader: number of pages: " + this.numberOfPages();
        } else {
            return "Jbig2SegmentReader in indeterminate state.";
        }
    }
}