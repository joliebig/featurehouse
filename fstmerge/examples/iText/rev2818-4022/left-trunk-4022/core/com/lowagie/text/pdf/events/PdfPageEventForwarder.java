

package com.lowagie.text.pdf.events;

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.Document;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfPageEvent;
import com.lowagie.text.pdf.PdfWriter;



public class PdfPageEventForwarder implements PdfPageEvent {

    
    protected ArrayList events = new ArrayList();
    
    
    public void addPageEvent(PdfPageEvent event) {
        events.add(event);
    }
    
    
    public void onOpenDocument(PdfWriter writer, Document document) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onOpenDocument(writer, document);
        }
    }

    
    public void onStartPage(PdfWriter writer, Document document) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onStartPage(writer, document);
        }
    }

    
    public void onEndPage(PdfWriter writer, Document document) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onEndPage(writer, document);
        }
    }

    
    public void onCloseDocument(PdfWriter writer, Document document) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onCloseDocument(writer, document);
        }
    }

    
    public void onParagraph(PdfWriter writer, Document document,
            float paragraphPosition) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onParagraph(writer, document, paragraphPosition);
        }
    }

    
    public void onParagraphEnd(PdfWriter writer, Document document,
            float paragraphPosition) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onParagraphEnd(writer, document, paragraphPosition);
        }
    }

    
    public void onChapter(PdfWriter writer, Document document,
            float paragraphPosition, Paragraph title) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onChapter(writer, document, paragraphPosition, title);
        }
    }

    
    public void onChapterEnd(PdfWriter writer, Document document, float position) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onChapterEnd(writer, document, position);
        }
    }

    
    public void onSection(PdfWriter writer, Document document,
            float paragraphPosition, int depth, Paragraph title) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onSection(writer, document, paragraphPosition, depth, title);
        }
    }

    
    public void onSectionEnd(PdfWriter writer, Document document, float position) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onSectionEnd(writer, document, position);
        }
    }

    
    public void onGenericTag(PdfWriter writer, Document document,
            Rectangle rect, String text) {
        PdfPageEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPageEvent)i.next();
            event.onGenericTag(writer, document, rect, text);
        }
    }
}