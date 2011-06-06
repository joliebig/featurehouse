

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Chunk;
import com.lowagie.text.Font;



abstract class AbstractRtfField extends Chunk implements RtfField {
    private static final byte[] fldDirty = "\\flddirty".getBytes();
    private static final byte[] fldPriv = "\\fldpriv".getBytes();
    private static final byte[] fldLock = "\\fldlock".getBytes();
    private static final byte[] fldEdit = "\\fldedit".getBytes();
    private static final byte[] fldAlt = "\\fldalt".getBytes();

    
    public AbstractRtfField(String content, Font font) {
        super(content, font);
    }

    
    private boolean rtfFieldIsLocked = false;

    
    private boolean rtfFieldIsDirty = false;

    
    private boolean rtfFieldWasEdited = false;

    
    private boolean rtfFieldIsPrivate = false;

    
    private boolean rtfFieldIsAlt = false;

    
    public final boolean isLocked() {
        return this.rtfFieldIsLocked;
    }

    
    public final void setLocked(final boolean rtfFieldIsLocked) {
        this.rtfFieldIsLocked = rtfFieldIsLocked;
    }

    
    public final void setDirty(final boolean rtfFieldIsDirty) {
        this.rtfFieldIsDirty = rtfFieldIsDirty;
    }

    
    public final boolean isDirty() {
        return this.rtfFieldIsDirty;
    }

    
    public final void setEdited(final boolean rtfFieldWasEdited) {
        this.rtfFieldWasEdited = rtfFieldWasEdited;
    }

    
    public final boolean wasEdited() {
        return this.rtfFieldWasEdited;
    }

    
    public final void setPrivate(final boolean rtfFieldIsPrivate) {
        this.rtfFieldIsPrivate = rtfFieldIsPrivate;
    }

    
    public final boolean isPrivate() {
        return this.rtfFieldIsPrivate;
    }

    
    public abstract void writeRtfFieldInitializationStuff(OutputStream out) throws IOException;

    
    public abstract void writeRtfFieldResultStuff(OutputStream out) throws IOException;

    
    public final void setAlt(final boolean rtfFieldIsAlt) {
        this.rtfFieldIsAlt = rtfFieldIsAlt;
    }

    
    public final boolean isAlt() {
        return this.rtfFieldIsAlt;
    }

    
    public final String content() {
        return getContent();
    }

    
    public final String getContent() {
        return "";
    }

    
    public void write( RtfWriter writer, OutputStream out ) throws IOException {
        writeRtfFieldBegin(out);
        writeRtfFieldModifiers(out);
        writeRtfFieldInstBegin(out);
        writer.writeInitialFontSignature( out, this );
        writeRtfFieldInitializationStuff(out);
        writeRtfFieldInstEnd(out);
        writeRtfFieldResultBegin(out);
        writer.writeInitialFontSignature( out, this );
        writeRtfFieldResultStuff(out);
        writeRtfFieldResultEnd(out);
        writeRtfFieldEnd(out);
    }

    
    protected final void writeRtfFieldBegin(OutputStream out)  throws IOException {
        out.write(RtfWriter.openGroup);
        out.write(RtfWriter.escape);
        out.write(RtfWriter.field);
    }

    
    protected final void writeRtfFieldModifiers(OutputStream out) throws IOException {
        if (isDirty()) {
            out.write(fldDirty);
        }

        if (wasEdited()) {
            out.write(fldEdit);
        }

        if (isLocked()) {
            out.write(fldLock);
        }

        if (isPrivate()) {
            out.write(fldPriv);
        }
    }

    
    protected final void writeRtfFieldInstBegin(OutputStream out) throws IOException {
        out.write( RtfWriter.openGroup );        
        out.write( RtfWriter.escape );
        out.write( RtfWriter.fieldContent );
        out.write( RtfWriter.delimiter );
    }

    
    protected final void writeRtfFieldInstEnd(OutputStream out) throws IOException {
        if (isAlt()) {
            out.write( fldAlt );
            out.write( RtfWriter.delimiter );
        }

        out.write( RtfWriter.closeGroup );
    }

    
    protected final void writeRtfFieldResultBegin(OutputStream out) throws IOException {
        out.write( RtfWriter.openGroup );        
        out.write( RtfWriter.escape );
        out.write( RtfWriter.fieldDisplay );
        out.write( RtfWriter.delimiter );
    }

    
    protected final void writeRtfFieldResultEnd(OutputStream out) throws IOException {
        out.write( RtfWriter.delimiter );
        out.write( RtfWriter.closeGroup );
    }

    
    protected final void writeRtfFieldEnd(OutputStream out) throws IOException {
        out.write( RtfWriter.closeGroup );
    }
}
