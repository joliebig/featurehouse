

package com.lowagie.text.rtf.field;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Chunk;
import com.lowagie.text.Font;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.style.RtfFont;



public abstract class RtfField extends Chunk implements RtfBasicElement {

    
    private static final byte[] FIELD = "\\field".getBytes();
    
    private static final byte[] FIELD_DIRTY = "\\flddirty".getBytes();
    
    private static final byte[] FIELD_PRIVATE = "\\fldpriv".getBytes();
    
    private static final byte[] FIELD_LOCKED = "\\fldlock".getBytes();
    
    private static final byte[] FIELD_EDIT = "\\fldedit".getBytes();
    
    private static final byte[] FIELD_ALT = "\\fldalt".getBytes();
    
    private static final byte[] FIELD_INSTRUCTIONS = "\\*\\fldinst".getBytes();
    
    private static final byte[] FIELD_RESULT = "\\fldrslt".getBytes();

    
    private boolean fieldDirty = false;
    
    private boolean fieldEdit = false;
    
    private boolean fieldLocked = false;
    
    private boolean fieldPrivate = false;
    
    private boolean fieldAlt = false;
    
    private boolean inTable = false;
    
    private boolean inHeader = false;
    
    protected RtfDocument document = null;
    
    private RtfFont font = null;

    
    protected RtfField(RtfDocument doc) {
        this(doc, new Font());
    }
    
    
    protected RtfField(RtfDocument doc, Font font) {
        super("", font);
        this.document = doc;
        this.font = new RtfFont(this.document, font);
    }
    
    
    public void setRtfDocument(RtfDocument doc) {
        this.document = doc;
        this.font.setRtfDocument(this.document);
    }
    
    
    private byte[] writeFieldBegin() throws IOException 
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        writeFieldBegin(result);
        return result.toByteArray();
    }
    
    private void writeFieldBegin(OutputStream result) throws IOException 
    {
        result.write(OPEN_GROUP);
        result.write(FIELD);
        if(fieldDirty) result.write(FIELD_DIRTY);
        if(fieldEdit) result.write(FIELD_EDIT);
        if(fieldLocked) result.write(FIELD_LOCKED);
        if(fieldPrivate) result.write(FIELD_PRIVATE);
    }
    
    
    private byte[] writeFieldInstBegin() throws IOException
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        writeFieldInstBegin(result);
        return result.toByteArray();
    }
    
    private void writeFieldInstBegin(OutputStream result) throws IOException 
    {
        result.write(OPEN_GROUP);        
        result.write(FIELD_INSTRUCTIONS);
        result.write(DELIMITER);
    }
    
    
    protected abstract byte[] writeFieldInstContent() throws IOException;

    
    protected void writeFieldInstContent(OutputStream out) throws IOException
    {
        byte[] b = writeFieldInstContent();
        if(b != null) out.write(b);
    }
    
    
    private byte[] writeFieldInstEnd() throws IOException 
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        writeFieldInstEnd(result);
        return result.toByteArray();
    }
    
    private void writeFieldInstEnd(OutputStream result) throws IOException 
    {
        if(fieldAlt) {
            result.write(DELIMITER);
            result.write(FIELD_ALT);
        }
        result.write(CLOSE_GROUP);
    }
    
    
    private byte[] writeFieldResultBegin() throws IOException 
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        writeFieldResultBegin(result);
        return result.toByteArray();
    }
    
    private void writeFieldResultBegin(final OutputStream result) throws IOException 
    {
        result.write(OPEN_GROUP);
        result.write(FIELD_RESULT);
        result.write(DELIMITER);
    }
    
    
    protected abstract byte[] writeFieldResultContent() throws IOException;    
     
    protected void writeFieldResultContent(OutputStream out) throws IOException
    {
        byte[] b = writeFieldResultContent();
        if(b != null) out.write(b);
    }
    
    
    private byte[] writeFieldResultEnd() throws IOException 
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        writeFieldResultEnd(result);
        return result.toByteArray();
    }
     
    private void writeFieldResultEnd(final OutputStream result) throws IOException 
    {
        result.write(DELIMITER);
        result.write(CLOSE_GROUP);
    }
    
    
    private byte[] writeFieldEnd() throws IOException
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        writeFieldEnd(result);        
        return result.toByteArray();
    }
    
    private void writeFieldEnd(OutputStream result) throws IOException
    {
        result.write(CLOSE_GROUP);
    }
    
    
    
    public byte[] write()
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeContent(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
        
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(this.font.writeBegin());








        writeFieldBegin(result);
        writeFieldInstBegin(result);
        writeFieldInstContent(result);
        writeFieldInstEnd(result);
        writeFieldResultBegin(result);
        writeFieldResultContent(result);
        writeFieldResultEnd(result);
        writeFieldEnd(result);
        
        result.write(this.font.writeEnd());
    }        
        
    
    public boolean isFieldAlt() {
        return fieldAlt;
    }
    
    
    public void setFieldAlt(boolean fieldAlt) {
        this.fieldAlt = fieldAlt;
    }
    
    
    public boolean isFieldDirty() {
        return fieldDirty;
    }
    
    
    public void setFieldDirty(boolean fieldDirty) {
        this.fieldDirty = fieldDirty;
    }
    
    
    public boolean isFieldEdit() {
        return fieldEdit;
    }
    
    
    public void setFieldEdit(boolean fieldEdit) {
        this.fieldEdit = fieldEdit;
    }
    
    
    public boolean isFieldLocked() {
        return fieldLocked;
    }
    
    
    public void setFieldLocked(boolean fieldLocked) {
        this.fieldLocked = fieldLocked;
    }
    
    
    public boolean isFieldPrivate() {
        return fieldPrivate;
    }
    
    
    public void setFieldPrivate(boolean fieldPrivate) {
        this.fieldPrivate = fieldPrivate;
    }

    
    public void setInTable(boolean inTable) {
        this.inTable = inTable;
    }
    
    
    public void setInHeader(boolean inHeader) {
        this.inHeader = inHeader;
    }
    
    
    public boolean isEmpty() {
        return false;
    }
    
    
    public void setFont(Font font) {
        super.setFont(font);
        this.font = new RtfFont(this.document, font);
    }
}
