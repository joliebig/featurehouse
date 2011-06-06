

package com.lowagie.text.pdf;

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.lowagie.text.Anchor;
import com.lowagie.text.Annotation;
import com.lowagie.text.BadElementException;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Font;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Image;
import com.lowagie.text.List;
import com.lowagie.text.ListItem;
import com.lowagie.text.MarkedObject;
import com.lowagie.text.MarkedSection;
import com.lowagie.text.Meta;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Section;
import com.lowagie.text.SimpleTable;
import com.lowagie.text.Table;
import com.lowagie.text.pdf.collection.PdfCollection;
import com.lowagie.text.pdf.internal.PdfAnnotationsImp;
import com.lowagie.text.pdf.internal.PdfViewerPreferencesImp;



class PdfDocument extends Document {
    
    
    
    public static class PdfInfo extends PdfDictionary {
        
        
        
        PdfInfo() {
            super();
            addProducer();
            addCreationDate();
        }
        
        
        
        PdfInfo(String author, String title, String subject) {
            this();
            addTitle(title);
            addSubject(subject);
            addAuthor(author);
        }
        
        
        
        void addTitle(String title) {
            put(PdfName.TITLE, new PdfString(title, PdfObject.TEXT_UNICODE));
        }
        
        
        
        void addSubject(String subject) {
            put(PdfName.SUBJECT, new PdfString(subject, PdfObject.TEXT_UNICODE));
        }
        
        
        
        void addKeywords(String keywords) {
            put(PdfName.KEYWORDS, new PdfString(keywords, PdfObject.TEXT_UNICODE));
        }
        
        
        
        void addAuthor(String author) {
            put(PdfName.AUTHOR, new PdfString(author, PdfObject.TEXT_UNICODE));
        }
        
        
        
        void addCreator(String creator) {
            put(PdfName.CREATOR, new PdfString(creator, PdfObject.TEXT_UNICODE));
        }
        
        
        
        void addProducer() {
            put(PdfName.PRODUCER, new PdfString(getVersion()));
        }
        
        
        
        void addCreationDate() {
            PdfString date = new PdfDate();
            put(PdfName.CREATIONDATE, date);
            put(PdfName.MODDATE, date);
        }
        
        void addkey(String key, String value) {
            if (key.equals("Producer") || key.equals("CreationDate"))
                return;
            put(new PdfName(key), new PdfString(value, PdfObject.TEXT_UNICODE));
        }
    }
    
    
    
    static class PdfCatalog extends PdfDictionary {
        
        
        PdfWriter writer;
        
        
        
        PdfCatalog(PdfIndirectReference pages, PdfWriter writer) {
            super(CATALOG);
            this.writer = writer;
            put(PdfName.PAGES, pages);
        }
        
        
        void addNames(TreeMap localDestinations, ArrayList documentJavaScript, HashMap documentFileAttachment, PdfWriter writer) {
            if (localDestinations.isEmpty() && documentJavaScript.isEmpty() && documentFileAttachment.isEmpty())
                return;
            try {
                PdfDictionary names = new PdfDictionary();
                if (!localDestinations.isEmpty()) {
                    PdfArray ar = new PdfArray();
                    for (Iterator i = localDestinations.entrySet().iterator(); i.hasNext();) {
                        Map.Entry entry = (Map.Entry) i.next();
                        String name = (String) entry.getKey();
                        Object obj[] = (Object[]) entry.getValue();
                        PdfIndirectReference ref = (PdfIndirectReference)obj[1];
                        ar.add(new PdfString(name, null));
                        ar.add(ref);
                    }
                    PdfDictionary dests = new PdfDictionary();
                    dests.put(PdfName.NAMES, ar);
                    names.put(PdfName.DESTS, writer.addToBody(dests).getIndirectReference());
                }
                if (!documentJavaScript.isEmpty()) {
                    String s[] = new String[documentJavaScript.size()];
                    for (int k = 0; k < s.length; ++k)
                        s[k] = Integer.toHexString(k);
                    Arrays.sort(s);
                    PdfArray ar = new PdfArray();
                    for (int k = 0; k < s.length; ++k) {
                        ar.add(new PdfString(s[k]));
                        ar.add((PdfIndirectReference)documentJavaScript.get(k));
                    }
                    PdfDictionary js = new PdfDictionary();
                    js.put(PdfName.NAMES, ar);
                    names.put(PdfName.JAVASCRIPT, writer.addToBody(js).getIndirectReference());
                }
                if (!documentFileAttachment.isEmpty()) {
                    names.put(PdfName.EMBEDDEDFILES, writer.addToBody(PdfNameTree.writeTree(documentFileAttachment, writer)).getIndirectReference());
                }
                put(PdfName.NAMES, writer.addToBody(names).getIndirectReference());
            }
            catch (IOException e) {
                throw new ExceptionConverter(e);
            }
        }
        
        
        void setOpenAction(PdfAction action) {
            put(PdfName.OPENACTION, action);
        }
        
        
        
        void setAdditionalActions(PdfDictionary actions) {
            try {
                put(PdfName.AA, writer.addToBody(actions).getIndirectReference());
            } catch (Exception e) {
                throw new ExceptionConverter(e);
            }
        }
    }


    
    
    public PdfDocument() {
        super();
        addProducer();
        addCreationDate();
    }

    
    private PdfWriter writer;
    
    
    public void addWriter(PdfWriter writer) throws DocumentException {
        if (this.writer == null) {
            this.writer = writer;
            annotationsImp = new PdfAnnotationsImp(writer);
            return;
        }
        throw new DocumentException("You can only add a writer to a PdfDocument once.");
    }
    

    

    
    
    private PdfContentByte text;
    
    
    private PdfContentByte graphics;
    
    
    private float leading = 0;
    
    
    private int alignment = Element.ALIGN_LEFT;
    
    
    private float currentHeight = 0;
    
    
    private boolean isParagraph = true;
    
    
    private PdfAction anchorAction = null;
    
    
    public boolean add(Element element) throws DocumentException {
        if (writer != null && writer.isPaused()) {
            return false;
        }
        try {
            switch(element.type()) {
                
                case Element.HEADER:
                    info.addkey(((Meta)element).getName(), ((Meta)element).getContent());
                    break;
                case Element.TITLE:
                    info.addTitle(((Meta)element).getContent());
                    break;
                case Element.SUBJECT:
                    info.addSubject(((Meta)element).getContent());
                    break;
                case Element.KEYWORDS:
                    info.addKeywords(((Meta)element).getContent());
                    break;
                case Element.AUTHOR:
                    info.addAuthor(((Meta)element).getContent());
                    break;
                case Element.CREATOR:
                    info.addCreator(((Meta)element).getContent());
                    break;
                case Element.PRODUCER:
                    
                    info.addProducer();
                    break;
                case Element.CREATIONDATE:
                    
                    info.addCreationDate();
                    break;
                    
                
                case Element.CHUNK: {
                    
                    if (line == null) {
                        carriageReturn();
                    }
                    
                    
                    PdfChunk chunk = new PdfChunk((Chunk) element, anchorAction);
                    
                    {
                        PdfChunk overflow;
                        while ((overflow = line.add(chunk)) != null) {
                            carriageReturn();
                            chunk = overflow;
                            chunk.trimFirstSpace();
                        }
                    }
                    pageEmpty = false;
                    if (chunk.isAttribute(Chunk.NEWPAGE)) {
                        newPage();
                    }
                    break;
                }
                case Element.ANCHOR: {
                    Anchor anchor = (Anchor) element;
                    String url = anchor.getReference();
                    leading = anchor.getLeading();
                    if (url != null) {
                        anchorAction = new PdfAction(url);
                    }
                    
                    element.process(this);
                    anchorAction = null;
                    break;
                }
                case Element.ANNOTATION: {
                    if (line == null) {
                        carriageReturn();
                    }
                    Annotation annot = (Annotation) element;
                    Rectangle rect = new Rectangle(0, 0);
                    if (line != null)
                        rect = new Rectangle(annot.llx(indentRight() - line.widthLeft()), annot.lly(indentTop() - currentHeight), annot.urx(indentRight() - line.widthLeft() + 20), annot.ury(indentTop() - currentHeight - 20));
                    PdfAnnotation an = PdfAnnotationsImp.convertAnnotation(writer, annot, rect);
                    annotationsImp.addPlainAnnotation(an);
                    pageEmpty = false;
                    break;
                }
                case Element.PHRASE: {
                    
                    leading = ((Phrase) element).getLeading();
                    
                    element.process(this);
                    break;
                }
                case Element.PARAGRAPH: {
                    
                    Paragraph paragraph = (Paragraph) element;
                    
                    addSpacing(paragraph.spacingBefore(), leading, paragraph.getFont());
                    
                    
                    alignment = paragraph.getAlignment();
                    leading = paragraph.getTotalLeading();
                    carriageReturn();
                    
                    
                    if (currentHeight + line.height() + leading > indentTop() - indentBottom()) {
                        newPage();
                    }

                    indentation.indentLeft += paragraph.getIndentationLeft();
                    indentation.indentRight += paragraph.getIndentationRight();
                    carriageReturn();
     
                    PdfPageEvent pageEvent = writer.getPageEvent();
                    if (pageEvent != null && isParagraph)
                        pageEvent.onParagraph(writer, this, indentTop() - currentHeight);
                    
                    
                    if (paragraph.getKeepTogether()) {
                        PdfPTable table = new PdfPTable(1);
                        table.setWidthPercentage(100f);
                        PdfPCell cell = new PdfPCell();
                        cell.addElement(paragraph);
                        cell.setBorder(Table.NO_BORDER);
                        table.addCell(cell);
                        this.add(table);
                    }
                    else {
                        indentation.paragraph += paragraph.getIndentationLeft();
                        line.setExtraIndent(paragraph.getFirstLineIndent());
                        element.process(this);
                        indentation.paragraph -= paragraph.getIndentationLeft();
                    }
                    
                    addSpacing(paragraph.spacingAfter(), paragraph.getTotalLeading(), paragraph.getFont());

                    if (pageEvent != null && isParagraph)
                        pageEvent.onParagraphEnd(writer, this, indentTop() - currentHeight);
                    
                    alignment = Element.ALIGN_LEFT;
                    indentation.indentLeft -= paragraph.getIndentationLeft();
                    indentation.indentRight -= paragraph.getIndentationRight();
                    carriageReturn();
                    break;
                }
                case Element.SECTION:
                case Element.CHAPTER: {
                    
                    
                    Section section = (Section) element;
                    
                    boolean hasTitle = section.getTitle() != null;
                    
                    
                    if (section.isTriggerNewPage()) {
                        newPage();
                    }
                    
                    else {
                        newLine();
                    }

                    if (hasTitle) {
                    float fith = indentTop() - currentHeight;
                    int rotation = pageSize.getRotation();
                    if (rotation == 90 || rotation == 180)
                        fith = pageSize.getHeight() - fith;
                        PdfDestination destination = new PdfDestination(PdfDestination.FITH, fith);
                        while (currentOutline.level() >= section.getDepth()) {
                            currentOutline = currentOutline.parent();
                        }
                        PdfOutline outline = new PdfOutline(currentOutline, destination, section.getBookmarkTitle(), section.isBookmarkOpen());
                        currentOutline = outline;
                    }
                    
                    
                    carriageReturn();
                    indentation.indentLeft += section.getIndentationLeft();
                    indentation.indentRight += section.getIndentationRight();
                    indentation.sectionIndentLeft += section.getIndentationLeft();
                    indentation.sectionIndentRight += section.getIndentationRight();
                    
                    PdfPageEvent pageEvent = writer.getPageEvent();
                    if (pageEvent != null)
                        if (element.type() == Element.CHAPTER)
                            pageEvent.onChapter(writer, this, indentTop() - currentHeight, section.getTitle());
                        else
                            pageEvent.onSection(writer, this, indentTop() - currentHeight, section.getDepth(), section.getTitle());
                    
                    
                    if (hasTitle) {
                        isParagraph = false;
                        add(section.getTitle());
                        isParagraph = true;
                    }
                    indentation.indentLeft += section.getIndentation();
                    indentation.sectionIndentLeft += section.getIndentation();
                    
                    element.process(this);
                    
                    indentation.indentLeft -= section.getIndentationLeft() + section.getIndentation();
                    indentation.indentRight -= section.getIndentationRight();
                    indentation.sectionIndentLeft -= section.getIndentationLeft() + section.getIndentation();
                    indentation.sectionIndentRight -= section.getIndentationRight();
                    
                    if (pageEvent != null)
                        if (element.type() == Element.CHAPTER)
                            pageEvent.onChapterEnd(writer, this, indentTop() - currentHeight);
                        else
                            pageEvent.onSectionEnd(writer, this, indentTop() - currentHeight);
                    
                    break;
                }
                case Element.LIST: {
                    
                    List list = (List) element;
                    if (list.isAlignindent()) {
                        list.normalizeIndentation();
                    }
                    
                    indentation.listIndentLeft += list.getIndentationLeft();
                    indentation.indentRight += list.getIndentationRight();
                    
                    element.process(this);
                    
                    indentation.listIndentLeft -= list.getIndentationLeft();
                    indentation.indentRight -= list.getIndentationRight();
                    break;
                }
                case Element.LISTITEM: {
                    
                    ListItem listItem = (ListItem) element;
                    
                    addSpacing(listItem.spacingBefore(), leading, listItem.getFont());
                   
                    
                    alignment = listItem.getAlignment();
                    indentation.listIndentLeft += listItem.getIndentationLeft();
                    indentation.indentRight += listItem.getIndentationRight();
                    leading = listItem.getTotalLeading();
                    carriageReturn();
                    
                    
                    line.setListItem(listItem);
                    
                    element.process(this);

                    addSpacing(listItem.spacingAfter(), listItem.getTotalLeading(), listItem.getFont());
                   
                    
                    if (line.hasToBeJustified()) {
                        line.resetAlignment();
                    }
                    
                    carriageReturn();
                    indentation.listIndentLeft -= listItem.getIndentationLeft();
                    indentation.indentRight -= listItem.getIndentationRight();
                    break;
                }
                case Element.RECTANGLE: {
                    Rectangle rectangle = (Rectangle) element;
                    graphics.rectangle(rectangle);
                    pageEmpty = false;
                    break;
                }
                case Element.PTABLE: {
                    PdfPTable ptable = (PdfPTable)element;
                    if (ptable.size() <= ptable.getHeaderRows())
                        break; 

                    

                    indentation.indentLeft -= indentation.paragraph + indentation.sectionIndentLeft;
                    indentation.indentRight -= indentation.sectionIndentRight;
                    ensureNewLine();
                    flushLines();
                    indentation.indentLeft += indentation.paragraph + indentation.sectionIndentLeft;
                    indentation.indentRight += indentation.sectionIndentRight;
                    
                    addPTable(ptable);
                    pageEmpty = false;
                    newLine();
                    break;
                }
                case Element.MULTI_COLUMN_TEXT: {
                    ensureNewLine();
                    flushLines();
                    MultiColumnText multiText = (MultiColumnText) element;
                    float height = multiText.write(writer.getDirectContent(), this, indentTop() - currentHeight);
                    currentHeight += height;
                    text.moveText(0, -1f* height);
                    pageEmpty = false;
                    break;
                }
                case Element.TABLE : {
                    PdfTable table;
                    if (element instanceof PdfTable) {
                        
                        table = (PdfTable)element;
                        table.updateRowAdditions();
                    } else if (element instanceof SimpleTable) {
                        PdfPTable ptable = ((SimpleTable)element).createPdfPTable();
                        if (ptable.size() <= ptable.getHeaderRows())
                            break; 
                    
                        
                        ensureNewLine();
                        flushLines();
                        addPTable(ptable);                    
                        pageEmpty = false;
                        break;
                    } else if (element instanceof Table) {
                        try {
                               PdfPTable ptable = ((Table)element).createPdfPTable();
                               if (ptable.size() <= ptable.getHeaderRows())
                                   break; 
                     
                               
                               ensureNewLine();
                               flushLines();
                               addPTable(ptable);                    
                               pageEmpty = false;
                               break;
                        }
                        catch(BadElementException bee) {
                            
                            
                            float offset = ((Table)element).getOffset();
                            if (Float.isNaN(offset))
                                offset = leading;
                            carriageReturn();
                            lines.add(new PdfLine(indentLeft(), indentRight(), alignment, offset));
                            currentHeight += offset;
                            table = getPdfTable((Table)element, false);
                        }
                    } else {
                        return false;
                    }
                    add(table, false);
                    break;
                }
                case Element.JPEG:
                case Element.IMGRAW:
                case Element.IMGTEMPLATE: {
                    
                    add((Image) element);
                    break;
                }
                case Element.MARKED: {
                    MarkedObject mo;
                    if (element instanceof MarkedSection) {
                        mo = ((MarkedSection)element).title();
                        if (mo != null) {
                            mo.process(this);
                        }
                    }
                    mo = (MarkedObject)element;
                    mo.process(this);
                    break;
                }
                default:
                    return false;
            }
            lastElementType = element.type();
            return true;
        }
        catch(Exception e) {
            throw new DocumentException(e);
        }
    }


    
    
    public void open() {
        if (!open) {
            super.open();
            writer.open();
            rootOutline = new PdfOutline(writer);
            currentOutline = rootOutline;
        }
        try {
            initPage();
        }
        catch(DocumentException de) {
            throw new ExceptionConverter(de);
        }
    }


    
      
    public void close() {
        if (close) {
            return;
        }
        try {
            boolean wasImage = (imageWait != null);
            newPage();
            if (imageWait != null || wasImage) newPage();
            if (annotationsImp.hasUnusedAnnotations())
                throw new RuntimeException("Not all annotations could be added to the document (the document doesn't have enough pages).");
            PdfPageEvent pageEvent = writer.getPageEvent();
            if (pageEvent != null)
                pageEvent.onCloseDocument(writer, this);
            super.close();
            
            writer.addLocalDestinations(localDestinations);
            calculateOutlineCount();
            writeOutlines();
        }
        catch(Exception e) {
            throw new ExceptionConverter(e);
        }
        
        writer.close();
    }



    private boolean isNewpage = false;
    private int textEmptySize;
    
     
    public boolean newPage() {
        lastElementType = -1;
        isNewpage = true;
        if (writer == null || (writer.getDirectContent().size() == 0 && writer.getDirectContentUnder().size() == 0 && (pageEmpty || writer.isPaused()))) {
            return false;
        }
        if (!open || close) {
            throw new RuntimeException("The document isn't open.");
        }
        PdfPageEvent pageEvent = writer.getPageEvent();
        if (pageEvent != null)
            pageEvent.onEndPage(writer, this);
        
        
        super.newPage();
        
        
        indentation.imageIndentLeft = 0;
        indentation.imageIndentRight = 0;
        
        try {
            
            flushLines();
            
            
            
            
            int rotation = pageSize.getRotation();
            
            
            if (writer.isPdfX()) {
                if (thisBoxSize.containsKey("art") && thisBoxSize.containsKey("trim"))
                    throw new PdfXConformanceException("Only one of ArtBox or TrimBox can exist in the page.");
                if (!thisBoxSize.containsKey("art") && !thisBoxSize.containsKey("trim")) {
                    if (thisBoxSize.containsKey("crop"))
                        thisBoxSize.put("trim", thisBoxSize.get("crop"));
                    else
                        thisBoxSize.put("trim", new PdfRectangle(pageSize, pageSize.getRotation()));
                }
            }
            
            
            pageResources.addDefaultColorDiff(writer.getDefaultColorspace());        
            PdfDictionary resources = pageResources.getResources();
            
            
            
            PdfPage page = new PdfPage(new PdfRectangle(pageSize, rotation), thisBoxSize, resources, rotation);

            
            
            
            if (this.transition!=null) {
                page.put(PdfName.TRANS, this.transition.getTransitionDictionary());
                transition = null;
            }
            if (this.duration>0) {
                page.put(PdfName.DUR,new PdfNumber(this.duration));
                duration = 0;
            }
            if (pageAA != null) {
                page.put(PdfName.AA, writer.addToBody(pageAA).getIndirectReference());
                pageAA = null;
            }
            
            
            if (thumb != null) {
                page.put(PdfName.THUMB, thumb);
                thumb = null;
            }
            
            
            if (writer.getUserunit() > 0f) {
                page.put(PdfName.USERUNIT, new PdfNumber(writer.getUserunit()));
            }
            
            
            if (annotationsImp.hasUnusedAnnotations()) {
                PdfArray array = annotationsImp.rotateAnnotations(writer, pageSize);
                if (array.size() != 0)
                    page.put(PdfName.ANNOTS, array);
            }
            
            
            if (writer.isTagged())
                 page.put(PdfName.STRUCTPARENTS, new PdfNumber(writer.getCurrentPageNumber() - 1));
            
            if (text.size() > textEmptySize)
                text.endText();
            else
                text = null;
            writer.add(page, new PdfContents(writer.getDirectContentUnder(), graphics, text, writer.getDirectContent(), pageSize));
            
            initPage();
        }
        catch(DocumentException de) {
            
            throw new ExceptionConverter(de);
        }
        catch (IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
        isNewpage = false;
        return true;
    }


    
      
    public boolean setPageSize(Rectangle pageSize) {
        if (writer != null && writer.isPaused()) {
            return false;
        }
        nextPageSize = new Rectangle(pageSize);
        return true;
    }



    
    protected float nextMarginLeft;
    
    
    protected float nextMarginRight;
    
    
    protected float nextMarginTop;
    
    
    protected float nextMarginBottom;
    
    
    public boolean setMargins(float marginLeft, float marginRight, float marginTop, float marginBottom) {
        if (writer != null && writer.isPaused()) {
            return false;
        }
        nextMarginLeft = marginLeft;
        nextMarginRight = marginRight;
        nextMarginTop = marginTop;
        nextMarginBottom = marginBottom;
        return true;
    }


    
    
    public boolean setMarginMirroring(boolean MarginMirroring) {
        if (writer != null && writer.isPaused()) {
            return false;
        }
        return super.setMarginMirroring(MarginMirroring);
    }


    
     
    public void setPageCount(int pageN) {
        if (writer != null && writer.isPaused()) {
            return;
        }
        super.setPageCount(pageN);
    }


    
     
    public void resetPageCount() {
        if (writer != null && writer.isPaused()) {
            return;
        }
        super.resetPageCount();
    }


    
    
    public void setHeader(HeaderFooter header) {
        if (writer != null && writer.isPaused()) {
            return;
        }
        super.setHeader(header);
    }


    
    
    public void resetHeader() {
        if (writer != null && writer.isPaused()) {
            return;
        }
        super.resetHeader();
    }



    
    public void setFooter(HeaderFooter footer) {
        if (writer != null && writer.isPaused()) {
            return;
        }
        super.setFooter(footer);
    }


 
    
    public void resetFooter() {
        if (writer != null && writer.isPaused()) {
            return;
        }
        super.resetFooter();
    }
    


    
    private boolean firstPageEvent = true;
    
    
    private void initPage() throws DocumentException { 
        
        pageN++;

        
        annotationsImp.resetAnnotations();
        pageResources = new PageResources();
        
        writer.resetContent();
        graphics = new PdfContentByte(writer);
        text = new PdfContentByte(writer);
        text.reset();
        text.beginText();
        textEmptySize = text.size();

        markPoint = 0;
        if (marginMirroring && (getPageNumber() & 1) == 0) {
            marginRight = nextMarginLeft;
            marginLeft = nextMarginRight;
        }
        else {
            marginLeft = nextMarginLeft;
            marginRight = nextMarginRight;
        }
        marginTop = nextMarginTop;
        marginBottom = nextMarginBottom;
        imageEnd = -1;
        indentation.imageIndentRight = 0;
        indentation.imageIndentLeft = 0;
        indentation.indentBottom = 0;
        indentation.indentTop = 0;
        currentHeight = 0;
        
        
        pageSize = nextPageSize;
        thisBoxSize = new HashMap(boxSize);
        if (pageSize.getBackgroundColor() != null
        || pageSize.hasBorders()
        || pageSize.getBorderColor() != null) {
            add(pageSize);
        }

        float oldleading = leading;
        int oldAlignment = alignment;
        
        doFooter();
        
        text.moveText(left(), top());
        doHeader();
        pageEmpty = true;
        
        try {
            if (imageWait != null) {
                add(imageWait);
                imageWait = null;
            }
        }
        catch(Exception e) {
            throw new ExceptionConverter(e);
        }
        leading = oldleading;
        alignment = oldAlignment;
        carriageReturn();
        
        PdfPageEvent pageEvent = writer.getPageEvent();
        if (pageEvent != null) {
            if (firstPageEvent) {
                pageEvent.onOpenDocument(writer, this);
            }
            pageEvent.onStartPage(writer, this);
        }
        firstPageEvent = false;
    }

    
    private PdfLine line = null;
    
    private ArrayList lines = new ArrayList();
    
    
    private void newLine() throws DocumentException {
        lastElementType = -1;
        carriageReturn();
        if (lines != null && !lines.isEmpty()) {
            lines.add(line);
            currentHeight += line.height();
        }
        line = new PdfLine(indentLeft(), indentRight(), alignment, leading);
    }
    
      
    private void carriageReturn() {
        
        if (lines == null) {
            lines = new ArrayList();
        }
        
        if (line != null) {
            
            if (currentHeight + line.height() + leading < indentTop() - indentBottom()) {
                
                if (line.size() > 0) {
                    currentHeight += line.height();
                    lines.add(line);
                    pageEmpty = false;
                }
            }
            
            else {
                newPage();
            }
        }
        if (imageEnd > -1 && currentHeight > imageEnd) {
            imageEnd = -1;
            indentation.imageIndentRight = 0;
            indentation.imageIndentLeft = 0;
        }
        
        line = new PdfLine(indentLeft(), indentRight(), alignment, leading);
    }
    
    
    public float getVerticalPosition(boolean ensureNewLine) {
        
        if (ensureNewLine) {
          ensureNewLine();
        }
        return top() -  currentHeight - indentation.indentTop;
    }

    
    private int lastElementType = -1;
    
    
    private void ensureNewLine() {
      try {
        if ((lastElementType == Element.PHRASE) || 
            (lastElementType == Element.CHUNK)) {
          newLine();
          flushLines();
        }
      } catch (DocumentException ex) {
        throw new ExceptionConverter(ex);
        }
    }
    
     
    private float flushLines() throws DocumentException {
        
        if (lines == null) {
            return 0;
        }
        boolean newline=false;
        
        if (line != null && line.size() > 0) {
            lines.add(line);
            line = new PdfLine(indentLeft(), indentRight(), alignment, leading);
            newline=true;
        }
        
        
        if (lines.isEmpty()) {
            return 0;
        }
        
        
        Object currentValues[] = new Object[2];
        PdfFont currentFont = null;
        float displacement = 0;
        PdfLine l;
        Float lastBaseFactor = new Float(0);
        currentValues[1] = lastBaseFactor;
        
        for (Iterator i = lines.iterator(); i.hasNext(); ) {
                        
            
            l = (PdfLine) i.next();
            
            if(isNewpage && newline) { 
                newline=false;
                text.moveText(l.indentLeft() - indentLeft() + indentation.listIndentLeft + indentation.paragraph,-l.height());
            }
            else {
                text.moveText(l.indentLeft() - indentLeft() + indentation.listIndentLeft, -l.height());
            }
            
            
            if (l.listSymbol() != null) {
                ColumnText.showTextAligned(graphics, Element.ALIGN_LEFT, new Phrase(l.listSymbol()), text.getXTLM() - l.listIndent(), text.getYTLM(), 0);
            }
            
            currentValues[0] = currentFont;
            
            writeLineToContent(l, text, graphics, currentValues, writer.getSpaceCharRatio());
            
            currentFont = (PdfFont)currentValues[0];
            
            displacement += l.height();
            if (indentLeft() - indentation.listIndentLeft != l.indentLeft()) {
                text.moveText(indentLeft() - l.indentLeft() - indentation.listIndentLeft, 0);
            }
            
        }
        lines = new ArrayList();
        return displacement;
    }
    
    
    static final String hangingPunctuation = ".,;:'";
    
    
    void writeLineToContent(PdfLine line, PdfContentByte text, PdfContentByte graphics, Object currentValues[], float ratio)  throws DocumentException {
        PdfFont currentFont = (PdfFont)(currentValues[0]);
        float lastBaseFactor = ((Float)(currentValues[1])).floatValue();
        PdfChunk chunk;
        int numberOfSpaces;
        int lineLen;
        boolean isJustified;
        float hangingCorrection = 0;
        float hScale = 1;
        float lastHScale = Float.NaN;
        float baseWordSpacing = 0;
        float baseCharacterSpacing = 0;
        
        numberOfSpaces = line.numberOfSpaces();
        lineLen = line.toString().length();
        
        isJustified = line.hasToBeJustified() && (numberOfSpaces != 0 || lineLen > 1);
        if (isJustified) {
            if (line.isNewlineSplit() && line.widthLeft() >= (lastBaseFactor * (ratio * numberOfSpaces + lineLen - 1))) {
                if (line.isRTL()) {
                    text.moveText(line.widthLeft() - lastBaseFactor * (ratio * numberOfSpaces + lineLen - 1), 0);
                }
                baseWordSpacing = ratio * lastBaseFactor;
                baseCharacterSpacing = lastBaseFactor;
            }
            else {
                float width = line.widthLeft();
                PdfChunk last = line.getChunk(line.size() - 1);
                if (last != null) {
                    String s = last.toString();
                    char c;
                    if (s.length() > 0 && hangingPunctuation.indexOf((c = s.charAt(s.length() - 1))) >= 0) {
                        float oldWidth = width;
                        width += last.font().width(c) * 0.4f;
                        hangingCorrection = width - oldWidth;
                    }
                }
                float baseFactor = width / (ratio * numberOfSpaces + lineLen - 1);
                baseWordSpacing = ratio * baseFactor;
                baseCharacterSpacing = baseFactor;
                lastBaseFactor = baseFactor;
            }
        }
        
        int lastChunkStroke = line.getLastStrokeChunk();
        int chunkStrokeIdx = 0;
        float xMarker = text.getXTLM();
        float baseXMarker = xMarker;
        float yMarker = text.getYTLM();
        boolean adjustMatrix = false;
        
        
        for (Iterator j = line.iterator(); j.hasNext(); ) {
            chunk = (PdfChunk) j.next();
            Color color = chunk.color();
            hScale = 1;
            
            if (chunkStrokeIdx <= lastChunkStroke) {
                float width;
                if (isJustified) {
                    width = chunk.getWidthCorrected(baseCharacterSpacing, baseWordSpacing);
                }
                else
                    width = chunk.width();
                if (chunk.isStroked()) {
                    PdfChunk nextChunk = line.getChunk(chunkStrokeIdx + 1);
                    if (chunk.isAttribute(Chunk.BACKGROUND)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.BACKGROUND))
                            subtract = 0;
                        if (nextChunk == null)
                            subtract += hangingCorrection;
                        float fontSize = chunk.font().size();
                        float ascender = chunk.font().getFont().getFontDescriptor(BaseFont.ASCENT, fontSize);
                        float descender = chunk.font().getFont().getFontDescriptor(BaseFont.DESCENT, fontSize);
                        Object bgr[] = (Object[])chunk.getAttribute(Chunk.BACKGROUND);
                        graphics.setColorFill((Color)bgr[0]);
                        float extra[] = (float[])bgr[1];
                        graphics.rectangle(xMarker - extra[0],
                            yMarker + descender - extra[1] + chunk.getTextRise(),
                            width - subtract + extra[0] + extra[2],
                            ascender - descender + extra[1] + extra[3]);
                        graphics.fill();
                        graphics.setGrayFill(0);
                    }
                    if (chunk.isAttribute(Chunk.UNDERLINE)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.UNDERLINE))
                            subtract = 0;
                        if (nextChunk == null)
                            subtract += hangingCorrection;
                        Object unders[][] = (Object[][])chunk.getAttribute(Chunk.UNDERLINE);
                        Color scolor = null;
                        for (int k = 0; k < unders.length; ++k) {
                            Object obj[] = unders[k];
                            scolor = (Color)obj[0];
                            float ps[] = (float[])obj[1];
                            if (scolor == null)
                                scolor = color;
                            if (scolor != null)
                                graphics.setColorStroke(scolor);
                            float fsize = chunk.font().size();
                            graphics.setLineWidth(ps[0] + fsize * ps[1]);
                            float shift = ps[2] + fsize * ps[3];
                            int cap2 = (int)ps[4];
                            if (cap2 != 0)
                                graphics.setLineCap(cap2);
                            graphics.moveTo(xMarker, yMarker + shift);
                            graphics.lineTo(xMarker + width - subtract, yMarker + shift);
                            graphics.stroke();
                            if (scolor != null)
                                graphics.resetGrayStroke();
                            if (cap2 != 0)
                                graphics.setLineCap(0);
                        }
                        graphics.setLineWidth(1);
                    }
                    if (chunk.isAttribute(Chunk.ACTION)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.ACTION))
                            subtract = 0;
                        if (nextChunk == null)
                            subtract += hangingCorrection;
                        text.addAnnotation(new PdfAnnotation(writer, xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size(), (PdfAction)chunk.getAttribute(Chunk.ACTION)));
                    }
                    if (chunk.isAttribute(Chunk.REMOTEGOTO)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.REMOTEGOTO))
                            subtract = 0;
                        if (nextChunk == null)
                            subtract += hangingCorrection;
                        Object obj[] = (Object[])chunk.getAttribute(Chunk.REMOTEGOTO);
                        String filename = (String)obj[0];
                        if (obj[1] instanceof String)
                            remoteGoto(filename, (String)obj[1], xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size());
                        else
                            remoteGoto(filename, ((Integer)obj[1]).intValue(), xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size());
                    }
                    if (chunk.isAttribute(Chunk.LOCALGOTO)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.LOCALGOTO))
                            subtract = 0;
                        if (nextChunk == null)
                            subtract += hangingCorrection;
                        localGoto((String)chunk.getAttribute(Chunk.LOCALGOTO), xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size());
                    }
                    if (chunk.isAttribute(Chunk.LOCALDESTINATION)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.LOCALDESTINATION))
                            subtract = 0;
                        if (nextChunk == null)
                            subtract += hangingCorrection;
                        localDestination((String)chunk.getAttribute(Chunk.LOCALDESTINATION), new PdfDestination(PdfDestination.XYZ, xMarker, yMarker + chunk.font().size(), 0));
                    }
                    if (chunk.isAttribute(Chunk.GENERICTAG)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.GENERICTAG))
                            subtract = 0;
                        if (nextChunk == null)
                            subtract += hangingCorrection;
                        Rectangle rect = new Rectangle(xMarker, yMarker, xMarker + width - subtract, yMarker + chunk.font().size());
                        PdfPageEvent pev = writer.getPageEvent();
                        if (pev != null)
                            pev.onGenericTag(writer, this, rect, (String)chunk.getAttribute(Chunk.GENERICTAG));
                    }
                    if (chunk.isAttribute(Chunk.PDFANNOTATION)) {
                        float subtract = lastBaseFactor;
                        if (nextChunk != null && nextChunk.isAttribute(Chunk.PDFANNOTATION))
                            subtract = 0;
                        if (nextChunk == null)
                            subtract += hangingCorrection;
                        float fontSize = chunk.font().size();
                        float ascender = chunk.font().getFont().getFontDescriptor(BaseFont.ASCENT, fontSize);
                        float descender = chunk.font().getFont().getFontDescriptor(BaseFont.DESCENT, fontSize);
                        PdfAnnotation annot = PdfFormField.shallowDuplicate((PdfAnnotation)chunk.getAttribute(Chunk.PDFANNOTATION));
                        annot.put(PdfName.RECT, new PdfRectangle(xMarker, yMarker + descender, xMarker + width - subtract, yMarker + ascender));
                        text.addAnnotation(annot);
                    }
                    float params[] = (float[])chunk.getAttribute(Chunk.SKEW);
                    Float hs = (Float)chunk.getAttribute(Chunk.HSCALE);
                    if (params != null || hs != null) {
                        float b = 0, c = 0;
                        if (params != null) {
                            b = params[0];
                            c = params[1];
                        }
                        if (hs != null)
                            hScale = hs.floatValue();
                        text.setTextMatrix(hScale, b, c, 1, xMarker, yMarker);
                    }
                    if (chunk.isImage()) {
                        Image image = chunk.getImage();
                        float matrix[] = image.matrix();
                        matrix[Image.CX] = xMarker + chunk.getImageOffsetX() - matrix[Image.CX];
                        matrix[Image.CY] = yMarker + chunk.getImageOffsetY() - matrix[Image.CY];
                        graphics.addImage(image, matrix[0], matrix[1], matrix[2], matrix[3], matrix[4], matrix[5]);
                        text.moveText(xMarker + lastBaseFactor + image.getScaledWidth() - text.getXTLM(), 0);
                    }
                }
                xMarker += width;
                ++chunkStrokeIdx;
            }

            if (chunk.font().compareTo(currentFont) != 0) {
                currentFont = chunk.font();
                text.setFontAndSize(currentFont.getFont(), currentFont.size());
            }
            float rise = 0;
            Object textRender[] = (Object[])chunk.getAttribute(Chunk.TEXTRENDERMODE);
            int tr = 0;
            float strokeWidth = 1;
            Color strokeColor = null;
            Float fr = (Float)chunk.getAttribute(Chunk.SUBSUPSCRIPT);
            if (textRender != null) {
                tr = ((Integer)textRender[0]).intValue() & 3;
                if (tr != PdfContentByte.TEXT_RENDER_MODE_FILL)
                    text.setTextRenderingMode(tr);
                if (tr == PdfContentByte.TEXT_RENDER_MODE_STROKE || tr == PdfContentByte.TEXT_RENDER_MODE_FILL_STROKE) {
                    strokeWidth = ((Float)textRender[1]).floatValue();
                    if (strokeWidth != 1)
                        text.setLineWidth(strokeWidth);
                    strokeColor = (Color)textRender[2];
                    if (strokeColor == null)
                        strokeColor = color;
                    if (strokeColor != null)
                        text.setColorStroke(strokeColor);
                }
            }
            if (fr != null)
                rise = fr.floatValue();
            if (color != null)
                text.setColorFill(color);
            if (rise != 0)
                text.setTextRise(rise);
            if (chunk.isImage()) {
                adjustMatrix = true;
            }
            
            
            else if (isJustified && numberOfSpaces > 0 && chunk.isSpecialEncoding()) {
                if (hScale != lastHScale) {
                    lastHScale = hScale;
                    text.setWordSpacing(baseWordSpacing / hScale);
                    text.setCharacterSpacing(baseCharacterSpacing / hScale);
                }
                String s = chunk.toString();
                int idx = s.indexOf(' ');
                if (idx < 0)
                    text.showText(chunk.toString());
                else {
                    float spaceCorrection = - baseWordSpacing * 1000f / chunk.font.size() / hScale;
                    PdfTextArray textArray = new PdfTextArray(s.substring(0, idx));
                    int lastIdx = idx;
                    while ((idx = s.indexOf(' ', lastIdx + 1)) >= 0) {
                        textArray.add(spaceCorrection);
                        textArray.add(s.substring(lastIdx, idx));
                        lastIdx = idx;
                    }
                    textArray.add(spaceCorrection);
                    textArray.add(s.substring(lastIdx));
                    text.showText(textArray);
                }
            }
            else {
                if (isJustified && hScale != lastHScale) {
                    lastHScale = hScale;
                    text.setWordSpacing(baseWordSpacing / hScale);
                    text.setCharacterSpacing(baseCharacterSpacing / hScale);
                }
                text.showText(chunk.toString());
            }
            
            if (rise != 0)
                text.setTextRise(0);
            if (color != null)
                text.resetRGBColorFill();
            if (tr != PdfContentByte.TEXT_RENDER_MODE_FILL)
                text.setTextRenderingMode(PdfContentByte.TEXT_RENDER_MODE_FILL);
            if (strokeColor != null)
                text.resetRGBColorStroke();
            if (strokeWidth != 1)
                text.setLineWidth(1);            
            if (chunk.isAttribute(Chunk.SKEW) || chunk.isAttribute(Chunk.HSCALE)) {
                adjustMatrix = true;
                text.setTextMatrix(xMarker, yMarker);
            }
        }
        if (isJustified) {
            text.setWordSpacing(0);
            text.setCharacterSpacing(0);
            if (line.isNewlineSplit())
                lastBaseFactor = 0;
        }
        if (adjustMatrix)
            text.moveText(baseXMarker - text.getXTLM(), 0);
        currentValues[0] = currentFont;
        currentValues[1] = new Float(lastBaseFactor);
    }

    Indentation indentation = new Indentation();
    public static class Indentation {
        
        float paragraph = 0;
        
        
        private float indentLeft = 0;
        
        
        private float sectionIndentLeft = 0;
        
        
        private float listIndentLeft = 0;
        
        
        private float imageIndentLeft = 0;
        
        
        private float indentRight = 0;
        
        
        private float sectionIndentRight = 0;
        
        
        private float imageIndentRight = 0;
        
        
        private float indentTop = 0;
        
        
        private float indentBottom = 0;
    }
    
    
    
    private float indentLeft() {
        return left(indentation.indentLeft + indentation.listIndentLeft + indentation.imageIndentLeft);
    }
    
    
    
    private float indentRight() {
        return right(indentation.indentRight + indentation.imageIndentRight);
    }
    
    
    
    private float indentTop() {
        return top(indentation.indentTop);
    }
    
    
    
    float indentBottom() {
        return bottom(indentation.indentBottom);
    }
    
    
    private void addSpacing(float extraspace, float oldleading, Font f) {
        if (extraspace == 0) return;
        if (pageEmpty) return;
        if (currentHeight + line.height() + leading > indentTop() - indentBottom()) return;
        leading = extraspace;
        carriageReturn();
        Chunk space = new Chunk(" ", f);
        space.process(this);
        carriageReturn();
        leading = oldleading;
    }
    

    
    
    private PdfInfo info = new PdfInfo();
    
    
    
    PdfInfo getInfo() {
        return info;
    }
    
    
    
    PdfCatalog getCatalog(PdfIndirectReference pages) {
        PdfCatalog catalog = new PdfCatalog(pages, writer);
        
        
        if (rootOutline.getKids().size() > 0) {
            catalog.put(PdfName.PAGEMODE, PdfName.USEOUTLINES);
            catalog.put(PdfName.OUTLINES, rootOutline.indirectReference());
        }
        
        
        writer.getPdfVersion().addToCatalog(catalog);
        
        
        viewerPreferences.addToCatalog(catalog);
        
        
        if (pageLabels != null) {
            catalog.put(PdfName.PAGELABELS, pageLabels.getDictionary());
        }
        
        
        catalog.addNames(localDestinations, documentJavaScript, documentFileAttachment, writer);
        
        
        if (openActionName != null) {
            PdfAction action = getLocalGotoAction(openActionName);
            catalog.setOpenAction(action);
        }
        else if (openActionAction != null)
            catalog.setOpenAction(openActionAction);
        if (additionalActions != null)   {
            catalog.setAdditionalActions(additionalActions);
        }
        
        
        if (collection != null) {
            catalog.put(PdfName.COLLECTION, collection);
        }

        
        if (annotationsImp.hasValidAcroForm()) {
            try {
                catalog.put(PdfName.ACROFORM, writer.addToBody(annotationsImp.getAcroForm()).getIndirectReference());
            }
            catch (IOException e) {
                throw new ExceptionConverter(e);
            }
        }
        
        return catalog;
    }
    

    
    
    private PdfOutline rootOutline;
    
    
    private PdfOutline currentOutline;
    
    
    void addOutline(PdfOutline outline, String name) {
        localDestination(name, outline.getPdfDestination());
    }
    
    
    public PdfOutline getRootOutline() {
        return rootOutline;
    }

    
    
    void calculateOutlineCount() {
        if (rootOutline.getKids().size() == 0)
            return;
        traverseOutlineCount(rootOutline);
    }
    
    
    void traverseOutlineCount(PdfOutline outline) {
        ArrayList kids = outline.getKids();
        PdfOutline parent = outline.parent();
        if (kids.isEmpty()) {
            if (parent != null) {
                parent.setCount(parent.getCount() + 1);
            }
        }
        else {
            for (int k = 0; k < kids.size(); ++k) {
                traverseOutlineCount((PdfOutline)kids.get(k));
            }
            if (parent != null) {
                if (outline.isOpen()) {
                    parent.setCount(outline.getCount() + parent.getCount() + 1);
                }
                else {
                    parent.setCount(parent.getCount() + 1);
                    outline.setCount(-outline.getCount());
                }
            }
        }
    }
    
    
    void writeOutlines() throws IOException {
        if (rootOutline.getKids().size() == 0)
            return;
        outlineTree(rootOutline);
        writer.addToBody(rootOutline, rootOutline.indirectReference());
    }
    
    
    void outlineTree(PdfOutline outline) throws IOException {
        outline.setIndirectReference(writer.getPdfIndirectReference());
        if (outline.parent() != null)
            outline.put(PdfName.PARENT, outline.parent().indirectReference());
        ArrayList kids = outline.getKids();
        int size = kids.size();
        for (int k = 0; k < size; ++k)
            outlineTree((PdfOutline)kids.get(k));
        for (int k = 0; k < size; ++k) {
            if (k > 0)
                ((PdfOutline)kids.get(k)).put(PdfName.PREV, ((PdfOutline)kids.get(k - 1)).indirectReference());
            if (k < size - 1)
                ((PdfOutline)kids.get(k)).put(PdfName.NEXT, ((PdfOutline)kids.get(k + 1)).indirectReference());
        }
        if (size > 0) {
            outline.put(PdfName.FIRST, ((PdfOutline)kids.get(0)).indirectReference());
            outline.put(PdfName.LAST, ((PdfOutline)kids.get(size - 1)).indirectReference());
        }
        for (int k = 0; k < size; ++k) {
            PdfOutline kid = (PdfOutline)kids.get(k);
            writer.addToBody(kid, kid.indirectReference());
        }
    }
    

    
    
    protected PdfViewerPreferencesImp viewerPreferences = new PdfViewerPreferencesImp();
    
    void setViewerPreferences(int preferences) {
        this.viewerPreferences.setViewerPreferences(preferences);
    }

    
    void addViewerPreference(PdfName key, PdfObject value) {
        this.viewerPreferences.addViewerPreference(key, value);
    }
 


    protected PdfPageLabels pageLabels;
    
    void setPageLabels(PdfPageLabels pageLabels) {
        this.pageLabels = pageLabels;
    }
    

    
    
    void localGoto(String name, float llx, float lly, float urx, float ury) {
        PdfAction action = getLocalGotoAction(name);
        annotationsImp.addPlainAnnotation(new PdfAnnotation(writer, llx, lly, urx, ury, action));
    }
    
    
    void remoteGoto(String filename, String name, float llx, float lly, float urx, float ury) {
        annotationsImp.addPlainAnnotation(new PdfAnnotation(writer, llx, lly, urx, ury, new PdfAction(filename, name)));
    }
    
    
    void remoteGoto(String filename, int page, float llx, float lly, float urx, float ury) {
        addAnnotation(new PdfAnnotation(writer, llx, lly, urx, ury, new PdfAction(filename, page)));
    }
    
    
    void setAction(PdfAction action, float llx, float lly, float urx, float ury) {
        addAnnotation(new PdfAnnotation(writer, llx, lly, urx, ury, action));
    }
    
    
    private TreeMap localDestinations = new TreeMap();
    
    PdfAction getLocalGotoAction(String name) {
        PdfAction action;
        Object obj[] = (Object[])localDestinations.get(name);
        if (obj == null)
            obj = new Object[3];
        if (obj[0] == null) {
            if (obj[1] == null) {
                obj[1] = writer.getPdfIndirectReference();
            }
            action = new PdfAction((PdfIndirectReference)obj[1]);
            obj[0] = action;
            localDestinations.put(name, obj);
        }
        else {
            action = (PdfAction)obj[0];
        }
        return action;
    }
    
    
    boolean localDestination(String name, PdfDestination destination) {
        Object obj[] = (Object[])localDestinations.get(name);
        if (obj == null)
            obj = new Object[3];
        if (obj[2] != null)
            return false;
        obj[2] = destination;
        localDestinations.put(name, obj);
        destination.addPage(writer.getCurrentPage());
        return true;
    }
    
    
    private ArrayList documentJavaScript = new ArrayList();
    void addJavaScript(PdfAction js) {
        if (js.get(PdfName.JS) == null)
            throw new RuntimeException("Only JavaScript actions are allowed.");
        try {
            documentJavaScript.add(writer.addToBody(js).getIndirectReference());
        }
        catch (IOException e) {
            throw new ExceptionConverter(e);
        }
    }
    
    ArrayList getDocumentJavaScript() {
        return documentJavaScript;
    }
    
    private HashMap documentFileAttachment = new HashMap();

    void addFileAttachment(String description, PdfFileSpecification fs) throws IOException {
        if (description == null) {
            PdfString desc = (PdfString)fs.get(PdfName.DESC);
            if (desc == null) {
                description = ""; 
            }
            else {
                description = PdfEncodings.convertToString(desc.getBytes(), null);
            }
        }
        fs.addDescription(description, true);
        if (description.length() == 0)
            description = "Unnamed";
        String fn = PdfEncodings.convertToString(new PdfString(description, PdfObject.TEXT_UNICODE).getBytes(), null);
        int k = 0;
        while (documentFileAttachment.containsKey(fn)) {
            ++k;
            fn = PdfEncodings.convertToString(new PdfString(description + " " + k, PdfObject.TEXT_UNICODE).getBytes(), null);
        }
        documentFileAttachment.put(fn, fs.getReference());
    }
    
    HashMap getDocumentFileAttachment() {
        return documentFileAttachment;
    }
    

    
    private String openActionName;
    
    void setOpenAction(String name) {
        openActionName = name;
        openActionAction = null;
    }

    private PdfAction openActionAction;
    void setOpenAction(PdfAction action) {
        openActionAction = action;
        openActionName = null;
    }

    private PdfDictionary additionalActions;
    void addAdditionalAction(PdfName actionType, PdfAction action)  {
        if (additionalActions == null)  {
            additionalActions = new PdfDictionary();
        }
        if (action == null)
            additionalActions.remove(actionType);
        else
            additionalActions.put(actionType, action);
        if (additionalActions.size() == 0)
            additionalActions = null;
    }
    

    
    private PdfCollection collection;

    
    public void setCollection(PdfCollection collection) {
        this.collection = collection;
    }
    

    
    PdfAnnotationsImp annotationsImp;
    
    
    PdfAcroForm getAcroForm() {
        return annotationsImp.getAcroForm();
    }
    
    void setSigFlags(int f) {
        annotationsImp.setSigFlags(f);
    }
    
    void addCalculationOrder(PdfFormField formField) {
        annotationsImp.addCalculationOrder(formField);
    }
    
    void addAnnotation(PdfAnnotation annot) {
        pageEmpty = false;
        annotationsImp.addAnnotation(annot);
    }


    
    protected int markPoint;
    
    int getMarkPoint() {
        return markPoint;
    }
     
    void incMarkPoint() {
        ++markPoint;
    }
    

    
    
    protected Rectangle nextPageSize = null;
    
    
    protected HashMap thisBoxSize = new HashMap();
    
    
    protected HashMap boxSize = new HashMap();
    
    void setCropBoxSize(Rectangle crop) {
        setBoxSize("crop", crop);
    }
    
    void setBoxSize(String boxName, Rectangle size) {
        if (size == null)
            boxSize.remove(boxName);
        else
            boxSize.put(boxName, new PdfRectangle(size));
    }
    
    
    Rectangle getBoxSize(String boxName) {
        PdfRectangle r = (PdfRectangle)thisBoxSize.get(boxName);
        if (r != null) return r.getRectangle();
        return null;
    }



    
    private boolean pageEmpty = true;
    
    void setPageEmpty(boolean pageEmpty) {
        this.pageEmpty = pageEmpty;
    }
    

    
    
    protected int duration=-1; 
        
    
    protected PdfTransition transition=null; 
        
    
    void setDuration(int seconds) {
        if (seconds > 0)
            this.duration=seconds;
        else
            this.duration=-1;
    }
        
    
    void setTransition(PdfTransition transition) {
        this.transition=transition;
    }
    
    protected PdfDictionary pageAA = null;
    void setPageAction(PdfName actionType, PdfAction action) {
        if (pageAA == null) {
            pageAA = new PdfDictionary();
        }
        pageAA.put(actionType, action);
    }


    
    private PdfIndirectReference thumb;
    void setThumbnail(Image image) throws PdfException, DocumentException {
        thumb = writer.getImageReference(writer.addDirectImageSimple(image));
    }
    

    
    
    protected PageResources pageResources;
    
    PageResources getPageResources() {
        return pageResources;
    }
    

    
    
    private boolean strictImageSequence = false;   
    
    
    boolean isStrictImageSequence() {
        return this.strictImageSequence;
    }
    
    
    void setStrictImageSequence(boolean strictImageSequence) {
        this.strictImageSequence = strictImageSequence;
    }
    
    
    private float imageEnd = -1;
 
    
    public void clearTextWrap() {
        float tmpHeight = imageEnd - currentHeight;
        if (line != null) {
            tmpHeight += line.height();
        }
        if ((imageEnd > -1) && (tmpHeight > 0)) {
            carriageReturn();
            currentHeight += tmpHeight;
        }
    }
    
    
    private Image imageWait = null;
    
    
    
    private void add(Image image) throws PdfException, DocumentException {
        
        if (image.hasAbsoluteY()) {
            graphics.addImage(image);
            pageEmpty = false;
            return;
        }
        
        
        if (currentHeight != 0 && indentTop() - currentHeight - image.getScaledHeight() < indentBottom()) {
            if (!strictImageSequence && imageWait == null) {
                imageWait = image;
                return;
            }
            newPage();
            if (currentHeight != 0 && indentTop() - currentHeight - image.getScaledHeight() < indentBottom()) {
                imageWait = image;
                return;
            }
        }
        pageEmpty = false;
        
        if (image == imageWait)
            imageWait = null;
        boolean textwrap = (image.getAlignment() & Image.TEXTWRAP) == Image.TEXTWRAP
        && !((image.getAlignment() & Image.MIDDLE) == Image.MIDDLE);
        boolean underlying = (image.getAlignment() & Image.UNDERLYING) == Image.UNDERLYING;
        float diff = leading / 2;
        if (textwrap) {
            diff += leading;
        }
        float lowerleft = indentTop() - currentHeight - image.getScaledHeight() -diff;
        float mt[] = image.matrix();
        float startPosition = indentLeft() - mt[4];
        if ((image.getAlignment() & Image.RIGHT) == Image.RIGHT) startPosition = indentRight() - image.getScaledWidth() - mt[4];
        if ((image.getAlignment() & Image.MIDDLE) == Image.MIDDLE) startPosition = indentLeft() + ((indentRight() - indentLeft() - image.getScaledWidth()) / 2) - mt[4];
        if (image.hasAbsoluteX()) startPosition = image.getAbsoluteX();
        if (textwrap) {
            if (imageEnd < 0 || imageEnd < currentHeight + image.getScaledHeight() + diff) {
                imageEnd = currentHeight + image.getScaledHeight() + diff;
            }
            if ((image.getAlignment() & Image.RIGHT) == Image.RIGHT) {
                
                indentation.imageIndentRight += image.getScaledWidth() + image.getIndentationLeft();
            }
            else {
                
                indentation.imageIndentLeft += image.getScaledWidth() + image.getIndentationRight();
            }
        }
        else {
            if ((image.getAlignment() & Image.RIGHT) == Image.RIGHT) startPosition -= image.getIndentationRight();
            else if ((image.getAlignment() & Image.MIDDLE) == Image.MIDDLE) startPosition += image.getIndentationLeft() - image.getIndentationRight();
            else startPosition += image.getIndentationLeft();
        }
        graphics.addImage(image, mt[0], mt[1], mt[2], mt[3], startPosition, lowerleft - mt[5]);
        if (!(textwrap || underlying)) {
            currentHeight += image.getScaledHeight() + diff;
            flushLines();
            text.moveText(0, - (image.getScaledHeight() + diff));
            newLine();
        }
    }
   

    
    
    void addPTable(PdfPTable ptable) throws DocumentException {
        ColumnText ct = new ColumnText(writer.getDirectContent());
        if (currentHeight > 0) {
            Paragraph p = new Paragraph();
            p.setLeading(0);
            ct.addElement(p);
            
            
            if (ptable.getKeepTogether() && !fitsPage(ptable, 0f))  {
                newPage();
            }
        }
        ct.addElement(ptable);
        boolean he = ptable.isHeadersInEvent();
        ptable.setHeadersInEvent(true);
        int loop = 0;
        while (true) {
            ct.setSimpleColumn(indentLeft(), indentBottom(), indentRight(), indentTop() - currentHeight);
            int status = ct.go();
            if ((status & ColumnText.NO_MORE_TEXT) != 0) {
                text.moveText(0, ct.getYLine() - indentTop() + currentHeight);
                currentHeight = indentTop() - ct.getYLine();
                break;
            }
            if (indentTop() - currentHeight == ct.getYLine())
                ++loop;
            else
                loop = 0;
            if (loop == 3) {
                add(new Paragraph("ERROR: Infinite table loop"));
                break;
            }
            newPage();
        }
        ptable.setHeadersInEvent(he);
    }
    
    
    
    boolean fitsPage(PdfPTable table, float margin) {
        if (!table.isLockedWidth()) {
            float totalWidth = (indentRight() - indentLeft()) * table.getWidthPercentage() / 100;
            table.setTotalWidth(totalWidth);
        }
        
        ensureNewLine();
        return table.getTotalHeight() <= indentTop() - currentHeight - indentBottom() - margin;
    }


    
    

    PdfTable getPdfTable(Table table, boolean supportRowAdditions) {
        return new PdfTable(table, indentLeft(), indentRight(), indentTop() - currentHeight, supportRowAdditions);
    }
    
    
    private static class RenderingContext {
        float pagetop = -1;
        float oldHeight = -1;

        PdfContentByte cellGraphics = null;
        
        float lostTableBottom;
        
        float maxCellBottom;
        float maxCellHeight;
        
        Map rowspanMap;
        Map pageMap = new HashMap();
        
        public PdfTable table;
        
        
        public int consumeRowspan(PdfCell c) {
            if (c.rowspan() == 1) {
                return 1;
            }
            
            Integer i = (Integer) rowspanMap.get(c);
            if (i == null) {
                i = new Integer(c.rowspan());
            }
            
            i = new Integer(i.intValue() - 1);
            rowspanMap.put(c, i);

            if (i.intValue() < 1) {
                return 1;
            }
            return i.intValue();
        }

        
        public int currentRowspan(PdfCell c) {
            Integer i = (Integer) rowspanMap.get(c);
            if (i == null) {
                return c.rowspan();
            } else {
                return i.intValue();
            }
        }
        
        public int cellRendered(PdfCell cell, int pageNumber) {
            Integer i = (Integer) pageMap.get(cell);
            if (i == null) {
                i = new Integer(1);
            } else {
                i = new Integer(i.intValue() + 1);
            }
            pageMap.put(cell, i);

            Integer pageInteger = new Integer(pageNumber);
            Set set = (Set) pageMap.get(pageInteger);
            
            if (set == null) {
                set = new HashSet();
                pageMap.put(pageInteger, set);
            }
            
            set.add(cell);
            
            return i.intValue();
        }

        public int numCellRendered(PdfCell cell) {
            Integer i = (Integer) pageMap.get(cell);
            if (i == null) {
                i = new Integer(0);
            } 
            return i.intValue();
        }
        
        public boolean isCellRenderedOnPage(PdfCell cell, int pageNumber) {
            Integer pageInteger = new Integer(pageNumber);
            Set set = (Set) pageMap.get(pageInteger);
            
            if (set != null) {
                return set.contains(cell);
            }
            
            return false;
        }
    };

    
    private void add(PdfTable table, boolean onlyFirstPage) throws DocumentException {
        
        flushLines();
        
        RenderingContext ctx = new RenderingContext();
        ctx.pagetop = indentTop();
        ctx.oldHeight = currentHeight;
        ctx.cellGraphics = new PdfContentByte(writer);
        ctx.rowspanMap = new HashMap();
        ctx.table = table;
        
        
        PdfCell cell;

        
        ArrayList dataCells = table.getCells();
                
        ArrayList headercells = table.getHeaderCells();
        
        if (!headercells.isEmpty() && (dataCells.isEmpty() || dataCells.get(0) != headercells.get(0))) {
            ArrayList allCells = new ArrayList(dataCells.size()+headercells.size());
            allCells.addAll(headercells);
            allCells.addAll(dataCells);
            dataCells = allCells;
        }
        
        ArrayList cells = dataCells;
        ArrayList rows = extractRows(cells, ctx);
        boolean isContinue = false;
        while (!cells.isEmpty()) {
            
            ctx.lostTableBottom = 0;
                        
            
            boolean cellsShown = false;

            
            Iterator iterator = rows.iterator();
              
            boolean atLeastOneFits = false;
            while (iterator.hasNext()) {
                ArrayList row = (ArrayList) iterator.next();
                analyzeRow(rows, ctx);
                renderCells(ctx, row, table.hasToFitPageCells() & atLeastOneFits);
                                
                if (!mayBeRemoved(row)) {
                    break;
                }
                consumeRowspan(row, ctx);
                iterator.remove();
                atLeastOneFits = true;
            }


            cells.clear();
            Set opt = new HashSet();
            iterator = rows.iterator();
            while (iterator.hasNext()) {
                ArrayList row = (ArrayList) iterator.next();
                
                Iterator cellIterator = row.iterator();
                while (cellIterator.hasNext()) {
                    cell = (PdfCell) cellIterator.next();
                    
                    if (!opt.contains(cell)) {
                        cells.add(cell);
                        opt.add(cell);
                    }
                }
            }
            
            
            Rectangle tablerec = new Rectangle(table);
            tablerec.setBorder(table.getBorder());
            tablerec.setBorderWidth(table.getBorderWidth());
            tablerec.setBorderColor(table.getBorderColor());
            tablerec.setBackgroundColor(table.getBackgroundColor());
            PdfContentByte under = writer.getDirectContentUnder();
            under.rectangle(tablerec.rectangle(top(), indentBottom()));
            under.add(ctx.cellGraphics);
            
            
            tablerec.setBackgroundColor(null);
            tablerec = tablerec.rectangle(top(), indentBottom());
            tablerec.setBorder(table.getBorder());
            under.rectangle(tablerec);
            

            ctx.cellGraphics = new PdfContentByte(null);
            
            
            if (!rows.isEmpty()) {
                isContinue = true;
                graphics.setLineWidth(table.getBorderWidth());
                if (cellsShown && (table.getBorder() & Rectangle.BOTTOM) == Rectangle.BOTTOM) {
                    
                                
                    
                    Color tColor = table.getBorderColor();
                    if (tColor != null) {
                        graphics.setColorStroke(tColor);
                    }
                    graphics.moveTo(table.getLeft(), Math.max(table.getBottom(), indentBottom()));
                    graphics.lineTo(table.getRight(), Math.max(table.getBottom(), indentBottom()));
                    graphics.stroke();
                    if (tColor != null) {
                        graphics.resetRGBColorStroke();
                    }
                }
                            
                
                pageEmpty = false;
                float difference = ctx.lostTableBottom;

                
                newPage();
                
                
                float heightCorrection = 0;
                boolean somethingAdded = false;
                if (currentHeight > 0) {
                    heightCorrection = 6;
                    currentHeight += heightCorrection;
                    somethingAdded = true;
                    newLine();
                    flushLines();
                    indentation.indentTop = currentHeight - leading;
                    currentHeight = 0;
                }
                else {
                    flushLines();
                }
                            
                
                int size = headercells.size();
                if (size > 0) {
                    
                    cell = (PdfCell) headercells.get(0);
                    float oldTop = cell.getTop(0);
                    
                    for (int i = 0; i < size; i++) {
                        cell = (PdfCell) headercells.get(i);
                        
                        cell.setTop(indentTop() - oldTop + cell.getTop(0));
                        cell.setBottom(indentTop() - oldTop + cell.getBottom(0));
                        ctx.pagetop = cell.getBottom();
                        
                        ctx.cellGraphics.rectangle(cell.rectangle(indentTop(), indentBottom()));
                        
                        ArrayList images = cell.getImages(indentTop(), indentBottom());
                        for (Iterator im = images.iterator(); im.hasNext();) {
                            cellsShown = true;
                            Image image = (Image) im.next();
                            graphics.addImage(image);
                        }
                        lines = cell.getLines(indentTop(), indentBottom());
                        float cellTop = cell.getTop(indentTop());
                        text.moveText(0, cellTop-heightCorrection);
                        float cellDisplacement = flushLines() - cellTop+heightCorrection;
                        text.moveText(0, cellDisplacement);
                    }
                                
                    currentHeight = indentTop() - ctx.pagetop + table.cellspacing();
                    text.moveText(0, ctx.pagetop - indentTop() - currentHeight);
                }
                else {
                    if (somethingAdded) {
                        ctx.pagetop = indentTop();
                        text.moveText(0, -table.cellspacing());
                    }
                }
                ctx.oldHeight = currentHeight - heightCorrection;
                            
                
                size = Math.min(cells.size(), table.columns());
                int i = 0;
                while (i < size) {
                    cell = (PdfCell) cells.get(i);
                    if (cell.getTop(-table.cellspacing()) > ctx.lostTableBottom) {
                        float newBottom = ctx.pagetop - difference + cell.getBottom();
                        float neededHeight = cell.remainingHeight();
                        if (newBottom > ctx.pagetop - neededHeight) {
                            difference += newBottom - (ctx.pagetop - neededHeight);
                        }
                    }
                    i++;
                }
                size = cells.size();
                table.setTop(indentTop());
                table.setBottom(ctx.pagetop - difference + table.getBottom(table.cellspacing()));
                for (i = 0; i < size; i++) {
                    cell = (PdfCell) cells.get(i);
                    float newBottom = ctx.pagetop - difference + cell.getBottom();
                    float newTop = ctx.pagetop - difference + cell.getTop(-table.cellspacing());
                    if (newTop > indentTop() - currentHeight) {
                        newTop = indentTop() - currentHeight;
                    }
               
                    cell.setTop(newTop );
                    cell.setBottom(newBottom );
                }
                if (onlyFirstPage) {
                    break;
                }
            }
        }
        
        float tableHeight = table.getTop() - table.getBottom();
        
        
        if (isContinue) {
            currentHeight = tableHeight;
            text.moveText(0, -(tableHeight - (ctx.oldHeight * 2)));
        } else {
            currentHeight = ctx.oldHeight + tableHeight;
            text.moveText(0, -tableHeight);
        }
        
        pageEmpty = false;
    }
    
    private void analyzeRow(ArrayList rows, RenderingContext ctx) {
        ctx.maxCellBottom = indentBottom();

        
        int rowIndex = 0;

        ArrayList row = (ArrayList) rows.get(rowIndex);
        int maxRowspan = 1;
        Iterator iterator = row.iterator();
        while (iterator.hasNext()) {
            PdfCell cell = (PdfCell) iterator.next();
            maxRowspan = Math.max(ctx.currentRowspan(cell), maxRowspan);
        }
        rowIndex += maxRowspan;
        
        boolean useTop = true;
        if (rowIndex == rows.size()) {
            rowIndex = rows.size() - 1;
            useTop = false;
        }
        
        if (rowIndex < 0 || rowIndex >= rows.size()) return;
        
        row = (ArrayList) rows.get(rowIndex);
        iterator = row.iterator();
        while (iterator.hasNext()) {
            PdfCell cell = (PdfCell) iterator.next();
            Rectangle cellRect = cell.rectangle(ctx.pagetop, indentBottom());
            if (useTop) {
                ctx.maxCellBottom = Math.max(ctx.maxCellBottom, cellRect.getTop());
            } else {
                if (ctx.currentRowspan(cell) == 1) {
                    ctx.maxCellBottom = Math.max(ctx.maxCellBottom, cellRect.getBottom());
                }
            }
        }
    }

    private boolean mayBeRemoved(ArrayList row) {
        Iterator iterator = row.iterator();
        boolean mayBeRemoved = true;
        while (iterator.hasNext()) {
            PdfCell cell = (PdfCell) iterator.next();
           
            mayBeRemoved &= cell.mayBeRemoved();
        }
        return mayBeRemoved;
    }

    private void consumeRowspan(ArrayList row, RenderingContext ctx) {
        Iterator iterator = row.iterator();
        while (iterator.hasNext()) {
            PdfCell c = (PdfCell) iterator.next();
            ctx.consumeRowspan(c);
        }
    }
    
    private ArrayList extractRows(ArrayList cells, RenderingContext ctx) {
        PdfCell cell;
        PdfCell previousCell = null;
        ArrayList rows = new ArrayList();
        java.util.List rowCells = new ArrayList();
        
        Iterator iterator = cells.iterator();
        while (iterator.hasNext()) {
            cell = (PdfCell) iterator.next();

            boolean isAdded = false;

            boolean isEndOfRow = !iterator.hasNext();
            boolean isCurrentCellPartOfRow = !iterator.hasNext();
            
            if (previousCell != null) {
                if (cell.getLeft() <= previousCell.getLeft()) {
                    isEndOfRow = true;
                    isCurrentCellPartOfRow = false;
                }
            }
            
            if (isCurrentCellPartOfRow) {
                rowCells.add(cell);
                isAdded = true;
            }
            
            if (isEndOfRow) {
                if (!rowCells.isEmpty()) {
                    
                    rows.add(rowCells);
                }
                
                
                rowCells = new ArrayList();                
            }

            if (!isAdded) {
                rowCells.add(cell);
            }
            
            previousCell = cell;
        }
        
        if (!rowCells.isEmpty()) {
            rows.add(rowCells);
        }
        
        
        for (int i = rows.size() - 1; i >= 0; i--) {
            ArrayList row = (ArrayList) rows.get(i);
            
            for (int j = 0; j < row.size(); j++) {
                PdfCell c = (PdfCell) row.get(j);
                int rowspan = c.rowspan();                
                
                for (int k = 1; k < rowspan && rows.size() <= i+k; k++) {
                    ArrayList spannedRow = ((ArrayList) rows.get(i + k));
                    if (spannedRow.size() > j)
                        spannedRow.add(j, c);
                }
            }
        }
                
        return rows;
    }

    private void renderCells(RenderingContext ctx, java.util.List cells, boolean hasToFit) throws DocumentException {
        PdfCell cell;
        Iterator iterator;
        if (hasToFit) {
            iterator = cells.iterator();
            while (iterator.hasNext()) {
                cell = (PdfCell) iterator.next();
                if (!cell.isHeader()) {
                    if (cell.getBottom() < indentBottom()) return;
                }
            }
        }
        iterator = cells.iterator();
        
        while (iterator.hasNext()) {
            cell = (PdfCell) iterator.next();
            if (!ctx.isCellRenderedOnPage(cell, getPageNumber())) {

                float correction = 0;
                if (ctx.numCellRendered(cell) >= 1) {
                    correction = 1.0f;
                }
            
                lines = cell.getLines(ctx.pagetop, indentBottom() - correction);
                
                
                if (lines != null && !lines.isEmpty()) {
                    
                    
                    float cellTop = cell.getTop(ctx.pagetop - ctx.oldHeight);
                    text.moveText(0, cellTop);
                    float cellDisplacement = flushLines() - cellTop;
                    
                    text.moveText(0, cellDisplacement);
                    if (ctx.oldHeight + cellDisplacement > currentHeight) {
                        currentHeight = ctx.oldHeight + cellDisplacement;
                    }

                    ctx.cellRendered(cell, getPageNumber());
                } 
                float indentBottom = Math.max(cell.getBottom(), indentBottom());
                Rectangle tableRect = ctx.table.rectangle(ctx.pagetop, indentBottom());
                indentBottom = Math.max(tableRect.getBottom(), indentBottom);
                
                
                Rectangle cellRect = cell.rectangle(tableRect.getTop(), indentBottom);
                 
                if (cellRect.getHeight() > 0) {
                    ctx.lostTableBottom = indentBottom;
                    ctx.cellGraphics.rectangle(cellRect);
                }
    
                
                ArrayList images = cell.getImages(ctx.pagetop, indentBottom());
                for (Iterator i = images.iterator(); i.hasNext();) {
                    Image image = (Image) i.next();
                    graphics.addImage(image);
                }
                
            }
        }
    }

    
    
    boolean breakTableIfDoesntFit(PdfTable table) throws DocumentException {
        table.updateRowAdditions();
        
        if (!table.hasToFitPageTable() && table.getBottom() <= indentation.indentBottom) {
            
            add(table, true);
            return true;
        }
        return false;
    }
    
    
    
    float bottom(Table table) {
        
        PdfTable tmp = getPdfTable(table, false);
        return tmp.getBottom();
    }
    

    private void doFooter() throws DocumentException {
        if (footer == null) return;
        
        
        float tmpIndentLeft = indentation.indentLeft;
        float tmpIndentRight = indentation.indentRight;
        
        float tmpListIndentLeft = indentation.listIndentLeft;
        float tmpImageIndentLeft = indentation.imageIndentLeft;
        float tmpImageIndentRight = indentation.imageIndentRight;
        

        indentation.indentLeft = indentation.indentRight = 0;
        
        indentation.listIndentLeft = 0;
        indentation.imageIndentLeft = 0;
        indentation.imageIndentRight = 0;
        
        
        footer.setPageNumber(pageN);
        leading = footer.paragraph().getTotalLeading();
        add(footer.paragraph());
        
        indentation.indentBottom = currentHeight;
        text.moveText(left(), indentBottom());
        flushLines();
        text.moveText(-left(), -bottom());
        footer.setTop(bottom(currentHeight));
        footer.setBottom(bottom() - (0.75f * leading));
        footer.setLeft(left());
        footer.setRight(right());
        graphics.rectangle(footer);
        indentation.indentBottom = currentHeight + leading * 2;
        currentHeight = 0;
        
        indentation.indentLeft = tmpIndentLeft;
        indentation.indentRight = tmpIndentRight;
        
        indentation.listIndentLeft = tmpListIndentLeft;
        indentation.imageIndentLeft = tmpImageIndentLeft;
        indentation.imageIndentRight = tmpImageIndentRight;
        
        
    }
    
    private void doHeader() throws DocumentException {
        
        if (header == null) return;
        
        
        float tmpIndentLeft = indentation.indentLeft;
        float tmpIndentRight = indentation.indentRight;
        
        float tmpListIndentLeft = indentation.listIndentLeft;
        float tmpImageIndentLeft = indentation.imageIndentLeft;
        float tmpImageIndentRight = indentation.imageIndentRight;
        
        indentation.indentLeft = indentation.indentRight = 0;
        
        indentation.listIndentLeft = 0;
        indentation.imageIndentLeft = 0;
        indentation.imageIndentRight = 0;
        
        
        header.setPageNumber(pageN);
        leading = header.paragraph().getTotalLeading();
        text.moveText(0, leading);
        add(header.paragraph());
        newLine();
        indentation.indentTop = currentHeight - leading;
        header.setTop(top() + leading);
        header.setBottom(indentTop() + leading * 2 / 3);
        header.setLeft(left());
        header.setRight(right());
        graphics.rectangle(header);
        flushLines();
        currentHeight = 0;
        
        
        indentation.indentLeft = tmpIndentLeft;
        indentation.indentRight = tmpIndentRight;
        
        indentation.listIndentLeft = tmpListIndentLeft;
        indentation.imageIndentLeft = tmpImageIndentLeft;
        indentation.imageIndentRight = tmpImageIndentRight;
        
        
    }
}