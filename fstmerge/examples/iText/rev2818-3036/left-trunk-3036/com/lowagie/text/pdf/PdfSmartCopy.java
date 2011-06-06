
package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import java.util.Arrays;



public class PdfSmartCopy extends PdfCopy {

    
    private HashMap streamMap = null;

    
    public PdfSmartCopy(Document document, OutputStream os) throws DocumentException {
        super(document, os);
        this.streamMap = new HashMap();
    }
    
    protected PdfIndirectReference copyIndirect(PRIndirectReference in) throws IOException, BadPdfFormatException {
        PdfObject srcObj = PdfReader.getPdfObjectRelease(in);
        ByteStore streamKey = null;
        boolean validStream = false;
        if (srcObj.isStream() && !((PRStream)srcObj).contains(PdfName.BBOX)) {
            validStream = true;
            
            streamKey = new ByteStore((PRStream)srcObj);
            PdfIndirectReference streamRef = (PdfIndirectReference) streamMap.get(streamKey);
            if (streamRef != null) {
                return streamRef;
            }
        }

        PdfIndirectReference theRef;
        RefKey key = new RefKey(in);
        IndirectReferences iRef = (IndirectReferences) indirects.get(key);
        if (iRef != null) {
            theRef = iRef.getRef();
            if (iRef.getCopied()) {
                return theRef;
            }
        } else {
            theRef = body.getPdfIndirectReference();
            iRef = new IndirectReferences(theRef);
            indirects.put(key, iRef);
        }
        iRef.setCopied();

        if (validStream) {
            streamMap.put(streamKey, theRef);
        }

        PdfObject obj = copyObject(srcObj);
        addToBody(obj, theRef);
        return theRef;
    }

    static class ByteStore {
        private byte[] b;
        private int hash;
        
        ByteStore(PRStream str) throws IOException {
            byte[] streamContent = PdfReader.getStreamBytesRaw(str);
            Object[] keys = str.getKeys().toArray();
            Arrays.sort(keys);
            ByteBuffer bb = new ByteBuffer();
            for (int k = 0; k < keys.length; ++k) {
                bb.append(keys[k].toString());
            }
            bb.append(streamContent);
            this.b = bb.toByteArray();
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof ByteStore))
                return false;
            if (hashCode() != obj.hashCode())
                return false;
            return Arrays.equals(b, ((ByteStore)obj).b);
        }

        public int hashCode() {
            if (hash == 0) {
                int len = b.length;
                for (int k = 0; k < len; ++k) {
                    hash = hash * 31 + (b[k] & 0xff);
                }
            }
            return hash;
        }
    }
}