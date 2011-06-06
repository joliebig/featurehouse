

package com.lowagie.text.pdf.richmedia;

import java.io.IOException;
import java.util.HashMap;

import com.lowagie.text.Rectangle;
import com.lowagie.text.exceptions.IllegalPdfSyntaxException;
import com.lowagie.text.pdf.PdfAnnotation;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDeveloperExtension;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfFileSpecification;
import com.lowagie.text.pdf.PdfIndirectReference;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNameTree;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfWriter;


public class RichMediaAnnotation {
    
    protected PdfWriter writer;
    
    protected PdfAnnotation annot;
    
    protected PdfDictionary richMediaContent = null;
    
    protected PdfIndirectReference richMediaContentReference = null;
    
    protected PdfDictionary richMediaSettings = new PdfDictionary(PdfName.RICHMEDIASETTINGS);
    
    protected HashMap<String, PdfObject> assetsmap = null;
    
    protected PdfArray configurations = null;
    
    protected PdfArray views = null;
    
    
    public RichMediaAnnotation(PdfWriter writer, Rectangle rect) {
        this.writer = writer;
        annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.RICHMEDIA);
        richMediaContent = new PdfDictionary(PdfName.RICHMEDIACONTENT);
        assetsmap = new HashMap<String, PdfObject>();
        configurations = new PdfArray();
        views = new PdfArray();
    }

    
    public RichMediaAnnotation(PdfWriter writer, Rectangle rect, PdfIndirectReference richMediaContentReference) {
        this.richMediaContentReference = richMediaContentReference;
        richMediaContent = null;
        this.writer = writer;
        annot = new PdfAnnotation(writer, rect);
        annot.put(PdfName.SUBTYPE, PdfName.RICHMEDIA);
    }
    
    
    public PdfIndirectReference getRichMediaContentReference() {
        return richMediaContentReference;
    }

    
    public PdfIndirectReference addAsset(String name, PdfFileSpecification fs)
        throws IOException {
        if (assetsmap == null)
            throw new IllegalPdfSyntaxException(
                "You can't add assets to reused RichMediaContent.");
        PdfIndirectReference ref = writer.addToBody(fs).getIndirectReference();
        assetsmap.put(name, ref);
        return ref;
    }
    
    
    public PdfIndirectReference addAsset(String name, PdfIndirectReference ref) throws IOException {
        if (views == null)
            throw new IllegalPdfSyntaxException(
                "You can't add assets to reused RichMediaContent.");
        assetsmap.put(name, ref);
        return ref;
    }
    
    
    public PdfIndirectReference addConfiguration(RichMediaConfiguration configuration) throws IOException {
        if (configurations == null)
            throw new IllegalPdfSyntaxException(
                "You can't add configurations to reused RichMediaContent.");
        PdfIndirectReference ref = writer.addToBody(configuration).getIndirectReference();
        configurations.add(ref);
        return ref;
    }
    
    
    public PdfIndirectReference addConfiguration(PdfIndirectReference ref) throws IOException {
        if (configurations == null)
            throw new IllegalPdfSyntaxException(
                "You can't add configurations to reused RichMediaContent.");
        configurations.add(ref);
        return ref;
    }
    
    
    public PdfIndirectReference addView(PdfDictionary view) throws IOException {
        if (views == null)
            throw new IllegalPdfSyntaxException(
                "You can't add views to reused RichMediaContent.");
        PdfIndirectReference ref = writer.addToBody(view).getIndirectReference();
        views.add(ref);
        return ref;
    }
    
    
    public PdfIndirectReference addView(PdfIndirectReference ref) throws IOException {
        if (views == null)
            throw new IllegalPdfSyntaxException(
                "You can't add views to reused RichMediaContent.");
        views.add(ref);
        return ref;
    }

    
    public void setActivation(RichMediaActivation richMediaActivation) {
        richMediaSettings.put(PdfName.ACTIVATION, richMediaActivation);
    }
    
    
    public void setDeactivation(RichMediaDeactivation richMediaDeactivation) {
        richMediaSettings.put(PdfName.DEACTIVATION, richMediaDeactivation);
    }
    
    
    public PdfAnnotation createAnnotation() throws IOException {
        if (richMediaContent != null) {
            if (!assetsmap.isEmpty()) {
                PdfDictionary assets = PdfNameTree.writeTree(assetsmap, writer);
                richMediaContent.put(PdfName.ASSETS, writer.addToBody(assets).getIndirectReference());
            }
            if (configurations.size() > 0) {
                richMediaContent.put(PdfName.CONFIGURATION, writer.addToBody(configurations).getIndirectReference());
            }
            if (views.size() > 0) {
                richMediaContent.put(PdfName.VIEWS, writer.addToBody(views).getIndirectReference());
            }
            richMediaContentReference = writer.addToBody(richMediaContent).getIndirectReference();
        }
        writer.addDeveloperExtension(PdfDeveloperExtension.ADOBE_1_7_EXTENSIONLEVEL3);
        annot.put(PdfName.RICHMEDIACONTENT, richMediaContentReference);
        annot.put(PdfName.RICHMEDIASETTINGS, writer.addToBody(richMediaSettings).getIndirectReference());
        return annot;
    }
}
