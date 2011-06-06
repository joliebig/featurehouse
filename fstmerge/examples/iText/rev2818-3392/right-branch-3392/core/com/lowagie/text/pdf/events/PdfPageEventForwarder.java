

package com.lowagie.text.pdf.events;

import java.util.ArrayList;

import com.lowagie.text.Document;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfPageEvent;
import com.lowagie.text.pdf.PdfWriter;



public class PdfPageEventForwarder implements PdfPageEvent {

    
    protected ArrayList<PdfPageEvent> events = new ArrayList<PdfPageEvent>();
    
    
    public void addPageEvent(PdfPageEvent event) {
        events.add(event);
    }
    
    
    public void onOpenDocument(PdfWriter writer, Document document) {
        for (PdfPageEvent event: events) {
            event.onOpenDocument(writer, document);
        }
    }

    
    public void onStartPage(PdfWriter writer, Document document) {
        for (PdfPageEvent event: events) {
            event.onStartPage(writer, document);
        }
    }

    
    public void onEndPage(PdfWriter writer, Document document) {
        for (PdfPageEvent event: events) {
            event.onEndPage(writer, document);
        }
    }

    
    public void onCloseDocument(PdfWriter writer, Document document) {
        for (PdfPageEvent event: events) {
            event.onCloseDocument(writer, document);
        }
    }

    
    public void onParagraph(PdfWriter writer, Document document,
            float paragraphPosition) {
        for (PdfPageEvent event: events) {
            event.onParagraph(writer, document, paragraphPosition);
        }
    }

    
    public void onParagraphEnd(PdfWriter writer, Document document,
            float paragraphPosition) {
        for (PdfPageEvent event: events) {
            event.onParagraphEnd(writer, document, paragraphPosition);
        }
    }

    
    public void onChapter(PdfWriter writer, Document document,
            float paragraphPosition, Paragraph title) {
        for (PdfPageEvent event: events) {
            event.onChapter(writer, document, paragraphPosition, title);
        }
    }

    
    public void onChapterEnd(PdfWriter writer, Document document, float position) {
        for (PdfPageEvent event: events) {
            event.onChapterEnd(writer, document, position);
        }
    }

    
    public void onSection(PdfWriter writer, Document document,
            float paragraphPosition, int depth, Paragraph title) {
        for (PdfPageEvent event: events) {
            event.onSection(writer, document, paragraphPosition, depth, title);
        }
    }

    
    public void onSectionEnd(PdfWriter writer, Document document, float position) {
        for (PdfPageEvent event: events) {
            event.onSectionEnd(writer, document, position);
        }
    }

    
    public void onGenericTag(PdfWriter writer, Document document,
            Rectangle rect, String text) {
        for (PdfPageEvent event: events) {
            event.onGenericTag(writer, document, rect, text);
        }
    }
}