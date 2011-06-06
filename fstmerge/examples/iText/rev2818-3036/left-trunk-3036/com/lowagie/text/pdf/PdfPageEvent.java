

package com.lowagie.text.pdf;
import com.lowagie.text.Document;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Rectangle;



public interface PdfPageEvent {
    

    public void onOpenDocument(PdfWriter writer, Document document);
    

    public void onStartPage(PdfWriter writer, Document document);
    

    public void onEndPage(PdfWriter writer, Document document);
    

    public void onCloseDocument(PdfWriter writer, Document document);
    

    public void onParagraph(PdfWriter writer, Document document, float paragraphPosition);
    

    public void onParagraphEnd(PdfWriter writer,Document document,float paragraphPosition);
    

    public void onChapter(PdfWriter writer,Document document,float paragraphPosition, Paragraph title);
    

    public void onChapterEnd(PdfWriter writer,Document document,float paragraphPosition);
    

    public void onSection(PdfWriter writer,Document document,float paragraphPosition, int depth, Paragraph title);
    

    public void onSectionEnd(PdfWriter writer,Document document,float paragraphPosition);
    

    public void onGenericTag(PdfWriter writer, Document document, Rectangle rect, String text);
}
