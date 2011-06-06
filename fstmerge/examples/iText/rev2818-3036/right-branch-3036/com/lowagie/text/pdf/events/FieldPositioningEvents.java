
package com.lowagie.text.pdf.events;

import java.io.IOException;
import java.util.HashMap;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfFormField;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPCellEvent;
import com.lowagie.text.pdf.PdfPageEventHelper;
import com.lowagie.text.pdf.PdfRectangle;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.text.pdf.TextField;


public class FieldPositioningEvents extends PdfPageEventHelper implements PdfPCellEvent {

    
    protected HashMap<String, PdfFormField> genericChunkFields = new HashMap<String, PdfFormField>();

    
    protected PdfFormField cellField = null;
    
    
    protected PdfWriter fieldWriter = null;
    
    protected PdfFormField parent = null;
    
    
    public FieldPositioningEvents() {}
    
    
    public float padding;
    
    
    public void addField(String text, PdfFormField field) {
        genericChunkFields.put(text, field);
    }
    
    
    public FieldPositioningEvents(PdfWriter writer, PdfFormField field) {
        this.cellField = field;
        this.fieldWriter = writer;
    }  
    
    
    public FieldPositioningEvents(PdfFormField parent, PdfFormField field) {
        this.cellField = field;
        this.parent = parent;
    }
    
    
    public FieldPositioningEvents(PdfWriter writer, String text) throws IOException, DocumentException {
        this.fieldWriter = writer;
        TextField tf = new TextField(writer, new Rectangle(0, 0), text);
        tf.setFontSize(14);
        cellField = tf.getTextField();
    }   
    
    
    public FieldPositioningEvents(PdfWriter writer, PdfFormField parent, String text) throws IOException, DocumentException {
        this.parent = parent;
        TextField tf = new TextField(writer, new Rectangle(0, 0), text);
        tf.setFontSize(14);
        cellField = tf.getTextField();
    }  

    
    public void setPadding(float padding) {
        this.padding = padding;
    }
    
    
    public void setParent(PdfFormField parent) {
        this.parent = parent;
    }
    
    public void onGenericTag(PdfWriter writer, Document document,
            Rectangle rect, String text) {
        rect.setBottom(rect.getBottom() - 3);
        PdfFormField field = genericChunkFields.get(text);
        if (field == null) {
            TextField tf = new TextField(writer, new Rectangle(rect.getLeft(padding), rect.getBottom(padding), rect.getRight(padding), rect.getTop(padding)), text);
            tf.setFontSize(14);
            try {
                field = tf.getTextField();
            } catch (Exception e) {
                throw new ExceptionConverter(e);
            }
        }
        else {
            field.put(PdfName.RECT,  new PdfRectangle(rect.getLeft(padding), rect.getBottom(padding), rect.getRight(padding), rect.getTop(padding)));
        }
        if (parent == null)
            writer.addAnnotation(field);
        else
            parent.addKid(field);
    }

    
    public void cellLayout(PdfPCell cell, Rectangle rect, PdfContentByte[] canvases) {
        if (cellField == null || (fieldWriter == null && parent == null)) throw new ExceptionConverter(new IllegalArgumentException("You have used the wrong constructor for this FieldPositioningEvents class."));
        cellField.put(PdfName.RECT, new PdfRectangle(rect.getLeft(padding), rect.getBottom(padding), rect.getRight(padding), rect.getTop(padding)));
        if (parent == null)
            fieldWriter.addAnnotation(cellField);
        else
            parent.addKid(cellField);
    }
}