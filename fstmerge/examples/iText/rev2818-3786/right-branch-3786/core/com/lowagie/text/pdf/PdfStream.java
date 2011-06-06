

package com.lowagie.text.pdf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.Document;
import com.lowagie.text.ExceptionConverter;
import java.util.ArrayList;



public class PdfStream extends PdfDictionary {
    
    

    
    public static final int DEFAULT_COMPRESSION = -1;
    
    public static final int NO_COMPRESSION = 0;
    
    public static final int BEST_SPEED = 1;
    
    public static final int BEST_COMPRESSION = 9;
    
    

    protected boolean compressed = false;
    
    protected int compressionLevel = NO_COMPRESSION;
    
    protected ByteArrayOutputStream streamBytes = null;
    protected InputStream inputStream;
    protected PdfIndirectReference ref;
    protected int inputStreamLength = -1;
    protected PdfWriter writer;
    protected int rawLength;
        
    static final byte STARTSTREAM[] = DocWriter.getISOBytes("stream\n");
    static final byte ENDSTREAM[] = DocWriter.getISOBytes("\nendstream");
    static final int SIZESTREAM = STARTSTREAM.length + ENDSTREAM.length;

    
    

 
    public PdfStream(byte[] bytes) {
        super();
        type = STREAM;
        this.bytes = bytes;
        rawLength = bytes.length;
        put(PdfName.LENGTH, new PdfNumber(bytes.length));
    }
  
        
    public PdfStream(InputStream inputStream, PdfWriter writer) {
        super();
        type = STREAM;
        this.inputStream = inputStream;
        this.writer = writer;
        ref = writer.getPdfIndirectReference();
        put(PdfName.LENGTH, ref);
    }
  

    
    protected PdfStream() {
        super();
        type = STREAM;
    }
    
    
    public void writeLength() throws IOException {
        if (inputStream == null)
            throw new UnsupportedOperationException("writeLength() can only be called in a contructed PdfStream(InputStream,PdfWriter).");
        if (inputStreamLength == -1)
            throw new IOException("writeLength() can only be called after output of the stream body.");
        writer.addToBody(new PdfNumber(inputStreamLength), ref, false);
    }
    
    
    public int getRawLength() {
        return rawLength;
    }
    
    
    public void flateCompress() {
        flateCompress(DEFAULT_COMPRESSION);
    }
    
    
    public void flateCompress(int compressionLevel) {
        if (!Document.compress)
            return;
        
        if (compressed) {
            return;
        }
        this.compressionLevel = compressionLevel;
        if (inputStream != null) {
            compressed = true;
            return;
        }
        
        PdfObject filter = PdfReader.getPdfObject(get(PdfName.FILTER));
        if (filter != null) {
            if (filter.isName()) {
                if (PdfName.FLATEDECODE.equals(filter))
                    return;
            }
            else if (filter.isArray()) {
                if (((PdfArray) filter).contains(PdfName.FLATEDECODE))
                    return;
            }
            else {
                throw new RuntimeException("Stream could not be compressed: filter is not a name or array.");
            }
        }
        try {
            
            ByteArrayOutputStream stream = new ByteArrayOutputStream();
            DeflaterOutputStream zip = new DeflaterOutputStream(stream, new Deflater(compressionLevel));
            if (streamBytes != null)
                streamBytes.writeTo(zip);
            else
                zip.write(bytes);
            zip.close();
            
            streamBytes = stream;
            bytes = null;
            put(PdfName.LENGTH, new PdfNumber(streamBytes.size()));
            if (filter == null) {
                put(PdfName.FILTER, PdfName.FLATEDECODE);
            }
            else {
                PdfArray filters = new PdfArray(filter);
                filters.add(PdfName.FLATEDECODE);
                put(PdfName.FILTER, filters);
            }
            compressed = true;
        }
        catch(IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }









    
    protected void superToPdf(PdfWriter writer, OutputStream os) throws IOException {
        super.toPdf(writer, os);
    }
    
    
    public void toPdf(PdfWriter writer, OutputStream os) throws IOException {
        if (inputStream != null && compressed)
            put(PdfName.FILTER, PdfName.FLATEDECODE);
        PdfEncryption crypto = null;
        if (writer != null)
            crypto = writer.getEncryption();
        if (crypto != null) {
            PdfObject filter = get(PdfName.FILTER);
            if (filter != null) {
                if (PdfName.CRYPT.equals(filter))
                    crypto = null;
                else if (filter.isArray()) {
                    ArrayList<PdfObject> af = ((PdfArray)filter).getArrayList();
                    if (!af.isEmpty() && PdfName.CRYPT.equals(af.get(0)))
                        crypto = null;
                }
            }
        }
        PdfObject nn = get(PdfName.LENGTH);
        if (crypto != null && nn != null && nn.isNumber()) {
            int sz = ((PdfNumber)nn).intValue();
            put(PdfName.LENGTH, new PdfNumber(crypto.calculateStreamSize(sz)));
            superToPdf(writer, os);
            put(PdfName.LENGTH, nn);
        }
        else
            superToPdf(writer, os);
        os.write(STARTSTREAM);
        if (inputStream != null) {
            rawLength = 0;
            DeflaterOutputStream def = null;
            OutputStreamCounter osc = new OutputStreamCounter(os);
            OutputStreamEncryption ose = null;
            OutputStream fout = osc;
            if (crypto != null && !crypto.isEmbeddedFilesOnly())
                fout = ose = crypto.getEncryptionStream(fout);
            if (compressed)    
                fout = def = new DeflaterOutputStream(fout, new Deflater(compressionLevel), 0x8000);
            
            byte buf[] = new byte[4192];
            while (true) {
                int n = inputStream.read(buf);
                if (n <= 0)
                    break;
                fout.write(buf, 0, n);
                rawLength += n;
            }
            if (def != null)
                def.finish();
            if (ose != null)
                ose.finish();
            inputStreamLength = osc.getCounter();
        }
        else {
            if (crypto != null && !crypto.isEmbeddedFilesOnly()) {
                byte b[];
                if (streamBytes != null) {
                    b = crypto.encryptByteArray(streamBytes.toByteArray());
                }
                else {
                    b = crypto.encryptByteArray(bytes);
                }
                os.write(b);
            }
            else {
                if (streamBytes != null)
                    streamBytes.writeTo(os);
                else
                    os.write(bytes);
            }
        }
        os.write(ENDSTREAM);
    }
    
        
    public void writeContent(OutputStream os) throws IOException {
        if (streamBytes != null)
            streamBytes.writeTo(os);
        else if (bytes != null)
            os.write(bytes);
    }
    
    
    public String toString() {
        if (get(PdfName.TYPE) == null) return "Stream";
        return "Stream of type: " + get(PdfName.TYPE);
    }
}
