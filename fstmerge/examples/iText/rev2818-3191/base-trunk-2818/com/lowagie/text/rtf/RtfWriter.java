

package com.lowagie.text.rtf;

import java.awt.Color;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.ListIterator;

import com.lowagie.text.Anchor;
import com.lowagie.text.Annotation;
import com.lowagie.text.Chunk;
import com.lowagie.text.DocWriter;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Font;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Image;
import com.lowagie.text.ListItem;
import com.lowagie.text.Meta;
import com.lowagie.text.PageSize;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Section;
import com.lowagie.text.SimpleTable;
import com.lowagie.text.Table;
import com.lowagie.text.pdf.codec.wmf.MetaDo;


public class RtfWriter extends DocWriter {
    

    

    
    public static final byte escape = (byte) '\\';

    
    private static final byte[] extendedEscape = "\\*\\".getBytes();

    
    protected static final byte delimiter = (byte) ' ';

    
    private static final byte commaDelimiter = (byte) ';';

    
    public static final byte openGroup = (byte) '{';

    
    public static final byte closeGroup = (byte) '}';

    

    
    private static final byte[] docBegin = "rtf1".getBytes();

    
    private static final byte[] ansi = "ansi".getBytes();

    
    private static final byte[] ansiCodepage = "ansicpg".getBytes();

    

    
    private static final byte[] fontTable = "fonttbl".getBytes();

    
    protected static final byte fontNumber = (byte) 'f';

    
    protected static final byte[] fontSize = "fs".getBytes();

    
    protected static final byte[] fontColor = "cf".getBytes();

    
    private static final byte[] fontModern = "fmodern".getBytes();

    
    private static final byte[] fontSwiss = "fswiss".getBytes();

    
    private static final byte[] fontRoman = "froman".getBytes();

    
    private static final byte[] fontTech = "ftech".getBytes();

    
    private static final byte[] fontCharset = "fcharset".getBytes();

    
    private static final byte[] fontCourier = "Courier".getBytes();

    
    private static final byte[] fontArial = "Arial".getBytes();

    
    private static final byte[] fontSymbol = "Symbol".getBytes();

    
    private static final byte[] fontTimesNewRoman = "Times New Roman".getBytes();

    
    private static final byte[] fontWindings = "Windings".getBytes();

    
    private static final byte[] defaultFont = "deff".getBytes();

    
    private static final byte[] firstIndent = "fi".getBytes();

    
    private static final byte[] listIndent = "li".getBytes();

    
    private static final byte[] rightIndent = "ri".getBytes();

    

    
    private static final byte[] sectionDefaults = "sectd".getBytes();

    
    private static final byte[] section = "sect".getBytes();

    
    public static final byte[] paragraphDefaults = "pard".getBytes();

    
    public static final byte[] paragraph = "par".getBytes();

    
    public static final byte[] sectionPageWidth = "pgwsxn".getBytes();

    
    public static final byte[] sectionPageHeight = "pghsxn".getBytes();

    

    
    private static final byte[] listtableGroup = "listtable".getBytes();

    
    private static final byte[] listoverridetableGroup = "listoverridetable".getBytes();

    
    private static final byte[] listDefinition = "list".getBytes();

    
    private static final byte[] listTemplateID = "listtemplateid".getBytes();

    
    private static final byte[] hybridList = "hybrid".getBytes();

    
    private static final byte[] listLevelDefinition = "listlevel".getBytes();

    
    private static final byte[] listLevelTypeOld = "levelnfc".getBytes();

    
    private static final byte[] listLevelTypeNew = "levelnfcn".getBytes();

    
    private static final byte[] listLevelAlignOld = "leveljc".getBytes();

    
    private static final byte[] listLevelAlignNew = "leveljcn".getBytes();

    
    private static final byte[] listLevelStartAt = "levelstartat".getBytes();

    
    private static final byte[] listLevelTextDefinition = "leveltext".getBytes();

    
    private static final byte[] listLevelTextLength = "\'0".getBytes();

    
    private static final byte[] listLevelTextStyleNumbers = "\'00.".getBytes();

    
    private static final byte[] listLevelTextStyleBullet = "u-3913 ?".getBytes();

    
    private static final byte[] listLevelNumbersDefinition = "levelnumbers".getBytes();

    
    private static final byte[] listLevelNumbers = "\\'0".getBytes();

    
    private static final byte[] tabStop = "tx".getBytes();

    
    private static final byte[] listBegin = "ls".getBytes();

    
    private static final byte[] listCurrentLevel = "ilvl".getBytes();

    
    private static final byte[] listTextOld = "listtext".getBytes();

    
    private static final byte[] tab = "tab".getBytes();

    
    private static final byte[] listBulletOld = "\'b7".getBytes();

    
    private static final byte[] listID = "listid".getBytes();

    
    private static final byte[] listOverride = "listoverride".getBytes();

    
    private static final byte[] listOverrideCount = "listoverridecount".getBytes();

    

    
    protected static final byte bold = (byte) 'b';

    
    protected static final byte italic = (byte) 'i';

    
    protected static final byte[] underline = "ul".getBytes();

    
    protected static final byte[] strikethrough = "strike".getBytes();

    
    public static final byte[] alignLeft = "ql".getBytes();

    
    public static final byte[] alignCenter = "qc".getBytes();

    
    public static final byte[] alignRight = "qr".getBytes();

    
    public static final byte[] alignJustify = "qj".getBytes();

    

    
    private static final byte[] colorTable = "colortbl".getBytes();

    
    private static final byte[] colorRed = "red".getBytes();

    
    private static final byte[] colorGreen = "green".getBytes();

    
    private static final byte[] colorBlue = "blue".getBytes();

    

    
    private static final byte[] infoBegin = "info".getBytes();

    
    private static final byte[] metaAuthor = "author".getBytes();

    
    private static final byte[] metaSubject = "subject".getBytes();

    
    private static final byte[] metaKeywords = "keywords".getBytes();

    
    private static final byte[] metaTitle = "title".getBytes();

    
    private static final byte[] metaProducer = "operator".getBytes();

    
    private static final byte[] metaCreationDate = "creationdate".getBytes();

    
    private static final byte[] year = "yr".getBytes();

    
    private static final byte[] month = "mo".getBytes();

    
    private static final byte[] day = "dy".getBytes();

    
    private static final byte[] hour = "hr".getBytes();

    
    private static final byte[] minute = "min".getBytes();

    
    private static final byte[] second = "sec".getBytes();

    
    private static final byte[] startSuper = "super".getBytes();

    
    private static final byte[] startSub = "sub".getBytes();

    
    private static final byte[] endSuperSub = "nosupersub".getBytes();

    

    
    private static final byte[] titlePage = "titlepg".getBytes();

    
    private static final byte[] facingPages = "facingp".getBytes();

    
    private static final byte[] headerBegin = "header".getBytes();

    
    private static final byte[] footerBegin = "footer".getBytes();

    
    private static final byte[] headerlBegin = "headerl".getBytes();

    private static final byte[] footerlBegin = "footerl".getBytes();

    private static final byte[] headerrBegin = "headerr".getBytes();

    private static final byte[] footerrBegin = "footerr".getBytes();

    private static final byte[] headerfBegin = "headerf".getBytes();

    private static final byte[] footerfBegin = "footerf".getBytes();

    

    
    private static final byte[] rtfPaperWidth = "paperw".getBytes();

    
    private static final byte[] rtfPaperHeight = "paperh".getBytes();

    
    private static final byte[] rtfMarginLeft = "margl".getBytes();

    
    private static final byte[] rtfMarginRight = "margr".getBytes();

    
    private static final byte[] rtfMarginTop = "margt".getBytes();

    
    private static final byte[] rtfMarginBottom = "margb".getBytes();

    
    private static final byte[] newPage = "page".getBytes();

    
    private static final byte[] landscapeTag1 = "landscape".getBytes();

    
    private static final byte[] landscapeTag2 = "lndscpsxn".getBytes();

    

    
    private static final byte[] annotationID = "atnid".getBytes();

    
    private static final byte[] annotationAuthor = "atnauthor".getBytes();

    
    private static final byte[] annotation = "annotation".getBytes();

    

    
    private static final byte[] pictureGroup = "shppict".getBytes();

    
    private static final byte[] picture = "pict".getBytes();

    
    private static final byte[] picturePNG = "pngblip".getBytes();

    
    private static final byte[] pictureJPEG = "jpegblip".getBytes();

    
    private static final byte[] pictureBMP = "dibitmap0".getBytes();

    
    private static final byte[] pictureWMF = "wmetafile8".getBytes();

    
    private static final byte[] pictureWidth = "picw".getBytes();

    
    private static final byte[] pictureHeight = "pich".getBytes();

    
    private static final byte[] pictureScaleX = "picscalex".getBytes();

    
    private static final byte[] pictureScaleY = "picscaley".getBytes();

    

    
    protected static final byte[] field = "field".getBytes();

    
    protected static final byte[] fieldContent = "fldinst".getBytes();

    
    protected static final byte[] fieldPage = "PAGE".getBytes();

    
    protected static final byte[] fieldHyperlink = "HYPERLINK".getBytes();

    
    protected static final byte[] fieldDisplay = "fldrslt".getBytes();


    

    

    
    private ArrayList fontList = new ArrayList();

    
    private ArrayList colorList = new ArrayList();

    
    private ByteArrayOutputStream content = null;

    
    private ByteArrayOutputStream info = null;

    
    private ByteArrayOutputStream listtable = null;

    
    private ByteArrayOutputStream listoverride = null;

    
    private HeaderFooter header = null;

    
    private HeaderFooter footer = null;

    
    private int marginLeft = 1800;

    
    private int marginRight = 1800;

    
    private int marginTop = 1440;

    
    private int marginBottom = 1440;

    
    private int pageWidth = 11906;

    
    private int pageHeight = 16838;

    
    public final static double TWIPSFACTOR = 20;

    
    private int currentListID = 1;

    
    private ArrayList listIds = null;

    
    private int listLevel = 0;

    
    private int maxListLevel = 0;

    
    private boolean writeTOC = false;

    
    private boolean hasTitlePage = false;

    
    private boolean inHeaderFooter = false;

    
    private boolean inTable = false;

    
    private boolean landscape = false;

    

    

    protected RtfWriter(Document doc, OutputStream os) {
        super(doc, os);
        document.addDocListener(this);
        initDefaults();
    }

    

    
    public void setGenerateTOCEntries(boolean writeTOC) {
        this.writeTOC = writeTOC;
    }

    
    public boolean getGeneratingTOCEntries() {
        return writeTOC;
    }

    
    public void setHasTitlePage(boolean hasTitlePage) {
        this.hasTitlePage = hasTitlePage;
    }

    
    public boolean getHasTitlePage() {
        return hasTitlePage;
    }

    
    public void setLandscape(boolean landscape) {
        this.landscape = landscape;
    }

    
    public boolean getLandscape() {
        return landscape;
    }

    

    
    public static RtfWriter getInstance(Document document, OutputStream os) {
        return new RtfWriter(document, os);
    }

    
    public void open() {
        super.open();
    }

    
    public void close() {
        if (open) {
            writeDocument();
            super.close();
        }
    }

    
    public void setFooter(HeaderFooter footer) {
        this.footer = footer;
        processHeaderFooter(this.footer);
    }

    
    public void setHeader(HeaderFooter header) {
        this.header = header;
        processHeaderFooter(this.header);
    }

    
    public void resetFooter() {
        setFooter(null);
    }

    
    public void resetHeader() {
        setHeader(null);
    }

    
    public boolean newPage() {
        try {
            content.write(escape);
            content.write(newPage);
            content.write(escape);
            content.write(paragraph);
        } catch (IOException e) {
            throw new ExceptionConverter(e);
        }
        return true;
    }

    
    public boolean setMargins(float marginLeft, float marginRight, float marginTop, float marginBottom) {
        this.marginLeft = (int) (marginLeft * TWIPSFACTOR);
        this.marginRight = (int) (marginRight * TWIPSFACTOR);
        this.marginTop = (int) (marginTop * TWIPSFACTOR);
        this.marginBottom = (int) (marginBottom * TWIPSFACTOR);
        return true;
    }

    
    public boolean setPageSize(Rectangle pageSize) {
        if (!parseFormat(pageSize, false)) {
            pageWidth = (int) (pageSize.getWidth() * TWIPSFACTOR);
            pageHeight = (int) (pageSize.getHeight() * TWIPSFACTOR);
            landscape = pageWidth > pageHeight;
        }
        return true;
    }

    
    public boolean writeTOC(String tocTitle, Font titleFont, boolean showTOCasEntry, Font showTOCEntryFont) {
        try {
            RtfTOC toc = new RtfTOC(tocTitle, titleFont);
            if (showTOCasEntry) {
                toc.addTOCAsTOCEntry(tocTitle, showTOCEntryFont);
            }
            add(new Paragraph(toc));
        } catch (DocumentException de) {
            return false;
        }
        return true;
    }

    
    public boolean add(Element element) throws DocumentException {
        if (pause) {
            return false;
        }
        return addElement(element, content);
    }


    

    
    protected boolean addElement(Element element, ByteArrayOutputStream out) throws DocumentException {
        try {
            switch (element.type()) {
                case Element.CHUNK:
                    writeChunk((Chunk) element, out);
                    break;
                case Element.PARAGRAPH:
                    writeParagraph((Paragraph) element, out);
                    break;
                case Element.ANCHOR:
                    writeAnchor((Anchor) element, out);
                    break;
                case Element.PHRASE:
                    writePhrase((Phrase) element, out);
                    break;
                case Element.CHAPTER:
                case Element.SECTION:
                    writeSection((Section) element, out);
                    break;
                case Element.LIST:
                    writeList((com.lowagie.text.List) element, out);
                    break;
                case Element.TABLE:
                    try {
                        writeTable((Table) element, out);
                    }
                    catch(ClassCastException cce) {
                        writeTable(((SimpleTable)element).createTable(), out);
                    }
                    break;
                case Element.ANNOTATION:
                    writeAnnotation((Annotation) element, out);
                    break;
                case Element.IMGRAW:
                case Element.IMGTEMPLATE:
                case Element.JPEG:
                    Image img = (Image)element;
                    writeImage(img, out);
                    break;

                case Element.AUTHOR:
                    writeMeta(metaAuthor, (Meta) element);
                    break;
                case Element.SUBJECT:
                    writeMeta(metaSubject, (Meta) element);
                    break;
                case Element.KEYWORDS:
                    writeMeta(metaKeywords, (Meta) element);
                    break;
                case Element.TITLE:
                    writeMeta(metaTitle, (Meta) element);
                    break;
                case Element.PRODUCER:
                    writeMeta(metaProducer, (Meta) element);
                    break;
                case Element.CREATIONDATE:
                    writeMeta(metaCreationDate, (Meta) element);
                    break;
            }
        } catch (IOException e) {
            return false;
        }
        return true;
    }

    
    private void writeSection(Section sectionElement, ByteArrayOutputStream out) throws IOException, DocumentException {
        if (sectionElement.type() == Element.CHAPTER) {
            out.write(escape);
            out.write(sectionDefaults);
            writeSectionDefaults(out);
        }
        if (sectionElement.getTitle() != null) {
            if (writeTOC) {
                StringBuffer title = new StringBuffer("");
                for (ListIterator li = sectionElement.getTitle().getChunks().listIterator(); li.hasNext();) {
                    title.append(((Chunk) li.next()).getContent());
                }
                add(new RtfTOCEntry(title.toString(), sectionElement.getTitle().getFont()));
            } else {
                add(sectionElement.getTitle());
            }
            out.write(escape);
            out.write(paragraph);
        }
        sectionElement.process(this);
        if (sectionElement.type() == Element.CHAPTER) {
            out.write(escape);
            out.write(section);
        }
        if (sectionElement.type() == Element.SECTION) {
            out.write(escape);
            out.write(paragraph);
        }
    }

    
    private void writeParagraph(Paragraph paragraphElement, ByteArrayOutputStream out) throws IOException {
        out.write(escape);
        out.write(paragraphDefaults);
        if (inTable) {
            out.write(escape);
            out.write(RtfCell.cellInTable);
        }
        switch (paragraphElement.getAlignment()) {
            case Element.ALIGN_LEFT:
                out.write(escape);
                out.write(alignLeft);
                break;
            case Element.ALIGN_RIGHT:
                out.write(escape);
                out.write(alignRight);
                break;
            case Element.ALIGN_CENTER:
                out.write(escape);
                out.write(alignCenter);
                break;
            case Element.ALIGN_JUSTIFIED:
            case Element.ALIGN_JUSTIFIED_ALL:
                out.write(escape);
                out.write(alignJustify);
                break;
        }
        out.write(escape);
        out.write(listIndent);
        writeInt(out, (int) (paragraphElement.getIndentationLeft() * TWIPSFACTOR));
        out.write(escape);
        out.write(rightIndent);
        writeInt(out, (int) (paragraphElement.getIndentationRight() * TWIPSFACTOR));
        Iterator chunks = paragraphElement.getChunks().iterator();
        while (chunks.hasNext()) {
            Chunk ch = (Chunk) chunks.next();
            ch.setFont(paragraphElement.getFont().difference(ch.getFont()));
        }
        ByteArrayOutputStream save = content;
        content = out;
        paragraphElement.process(this);
        content = save;
        if (!inTable) {
            out.write(escape);
            out.write(paragraph);
        }
    }

    
    private void writePhrase(Phrase phrase, ByteArrayOutputStream out) throws IOException {
        out.write(escape);
        out.write(paragraphDefaults);
        if (inTable) {
            out.write(escape);
            out.write(RtfCell.cellInTable);
        }
        Iterator chunks = phrase.getChunks().iterator();
        while (chunks.hasNext()) {
            Chunk ch = (Chunk) chunks.next();
            ch.setFont(phrase.getFont().difference(ch.getFont()));
        }
        ByteArrayOutputStream save = content;
        content = out;
        phrase.process(this);
        content = save;
    }

    
    private void writeAnchor(Anchor anchor, ByteArrayOutputStream out) throws IOException {
        if (anchor.getUrl() != null) {
            out.write(openGroup);
            out.write(escape);
            out.write(field);
            out.write(openGroup);
            out.write(extendedEscape);
            out.write(fieldContent);
            out.write(openGroup);
            out.write(fieldHyperlink);
            out.write(delimiter);
            out.write(anchor.getUrl().toString().getBytes());
            out.write(closeGroup);
            out.write(closeGroup);
            out.write(openGroup);
            out.write(escape);
            out.write(fieldDisplay);
            out.write(delimiter);
            writePhrase(anchor, out);
            out.write(closeGroup);
            out.write(closeGroup);
        } else {
            writePhrase(anchor, out);
        }
    }

    
    private void writeChunk(Chunk chunk, ByteArrayOutputStream out) throws IOException, DocumentException {
        if (chunk instanceof RtfField) {
            ((RtfField) chunk).write(this, out);
        } else {
            if (chunk.getImage() != null) {
                writeImage(chunk.getImage(), out);
            } else {
                writeInitialFontSignature(out, chunk);
                out.write(filterSpecialChar(chunk.getContent(), false).getBytes());
                writeFinishingFontSignature(out, chunk);
            }
        }
    }


    protected void writeInitialFontSignature(OutputStream out, Chunk chunk) throws IOException {
        Font font = chunk.getFont();

        out.write(escape);
        out.write(fontNumber);
        if (!font.getFamilyname().equalsIgnoreCase("unknown")) {
            writeInt(out, addFont(font));
        } else {
            writeInt(out, 0);
        }
        out.write(escape);
        out.write(fontSize);
        if (font.getSize() > 0) {
            writeInt(out, (int) (font.getSize() * 2));
        } else {
            writeInt(out, 20);
        }
        out.write(escape);
        out.write(fontColor);
        writeInt(out, addColor(font.getColor()));
        if (font.isBold()) {
            out.write(escape);
            out.write(bold);
        }
        if (font.isItalic()) {
            out.write(escape);
            out.write(italic);
        }
        if (font.isUnderlined()) {
            out.write(escape);
            out.write(underline);
        }
        if (font.isStrikethru()) {
            out.write(escape);
            out.write(strikethrough);
        }

        
        if (chunk.getAttributes() != null) {
            Float f = (Float) chunk.getAttributes().get(Chunk.SUBSUPSCRIPT);
            if (f != null)
                if (f.floatValue() > 0) {
                    out.write(escape);
                    out.write(startSuper);
                } else if (f.floatValue() < 0) {
                    out.write(escape);
                    out.write(startSub);
                }
        }

        out.write(delimiter);
    }


    protected void writeFinishingFontSignature(OutputStream out, Chunk chunk) throws IOException {
        Font font = chunk.getFont();

        if (font.isBold()) {
            out.write(escape);
            out.write(bold);
            writeInt(out, 0);
        }
        if (font.isItalic()) {
            out.write(escape);
            out.write(italic);
            writeInt(out, 0);
        }
        if (font.isUnderlined()) {
            out.write(escape);
            out.write(underline);
            writeInt(out, 0);
        }
        if (font.isStrikethru()) {
            out.write(escape);
            out.write(strikethrough);
            writeInt(out, 0);
        }

        
        if (chunk.getAttributes() != null) {
            Float f = (Float) chunk.getAttributes().get(Chunk.SUBSUPSCRIPT);
            if (f != null)
                if (f.floatValue() != 0) {
                    out.write(escape);
                    out.write(endSuperSub);
                }
        }
    }

    
    private void writeListElement(ListItem listItem, ByteArrayOutputStream out) throws IOException, DocumentException {
        Iterator chunks = listItem.getChunks().iterator();
        while (chunks.hasNext()) {
            Chunk ch = (Chunk) chunks.next();
            addElement(ch, out);
        }
        out.write(escape);
        out.write(paragraph);
    }

    
    private void writeList(com.lowagie.text.List list, ByteArrayOutputStream out) throws IOException, DocumentException {
        int type = 0;
        int align = 0;
        int fontNr = addFont(new Font(Font.SYMBOL, 10, Font.NORMAL, new Color(0, 0, 0)));
        if (!list.isNumbered()) type = 23;
        if (listLevel == 0) {
            maxListLevel = 0;
            listtable.write(openGroup);
            listtable.write(escape);
            listtable.write(listDefinition);
            int i = getRandomInt();
            listtable.write(escape);
            listtable.write(listTemplateID);
            writeInt(listtable, i);
            listtable.write(escape);
            listtable.write(hybridList);
            listtable.write((byte) '\n');
        }
        if (listLevel >= maxListLevel) {
            maxListLevel++;
            listtable.write(openGroup);
            listtable.write(escape);
            listtable.write(listLevelDefinition);
            listtable.write(escape);
            listtable.write(listLevelTypeOld);
            writeInt(listtable, type);
            listtable.write(escape);
            listtable.write(listLevelTypeNew);
            writeInt(listtable, type);
            listtable.write(escape);
            listtable.write(listLevelAlignOld);
            writeInt(listtable, align);
            listtable.write(escape);
            listtable.write(listLevelAlignNew);
            writeInt(listtable, align);
            listtable.write(escape);
            listtable.write(listLevelStartAt);
            writeInt(listtable, 1);
            listtable.write(openGroup);
            listtable.write(escape);
            listtable.write(listLevelTextDefinition);
            listtable.write(escape);
            listtable.write(listLevelTextLength);
            if (list.isNumbered()) {
                writeInt(listtable, 2);
            } else {
                writeInt(listtable, 1);
            }
            listtable.write(escape);
            if (list.isNumbered()) {
                listtable.write(listLevelTextStyleNumbers);
            } else {
                listtable.write(listLevelTextStyleBullet);
            }
            listtable.write(commaDelimiter);
            listtable.write(closeGroup);
            listtable.write(openGroup);
            listtable.write(escape);
            listtable.write(listLevelNumbersDefinition);
            if (list.isNumbered()) {
                listtable.write(delimiter);
                listtable.write(listLevelNumbers);
                writeInt(listtable, listLevel + 1);
            }
            listtable.write(commaDelimiter);
            listtable.write(closeGroup);
            if (!list.isNumbered()) {
                listtable.write(escape);
                listtable.write(fontNumber);
                writeInt(listtable, fontNr);
            }
            listtable.write(escape);
            listtable.write(firstIndent);
            writeInt(listtable, (int) (list.getIndentationLeft() * TWIPSFACTOR * -1));
            listtable.write(escape);
            listtable.write(listIndent);
            writeInt(listtable, (int) ((list.getIndentationLeft() + list.getSymbolIndent()) * TWIPSFACTOR));
            listtable.write(escape);
            listtable.write(rightIndent);
            writeInt(listtable, (int) (list.getIndentationRight() * TWIPSFACTOR));
            listtable.write(escape);
            listtable.write(tabStop);
            writeInt(listtable, (int) (list.getSymbolIndent() * TWIPSFACTOR));
            listtable.write(closeGroup);
            listtable.write((byte) '\n');
        }
        
        out.write(escape);
        out.write(paragraphDefaults);
        out.write(escape);
        out.write(alignLeft);
        out.write(escape);
        out.write(firstIndent);
        writeInt(out, (int) (list.getIndentationLeft() * TWIPSFACTOR * -1));
        out.write(escape);
        out.write(listIndent);
        writeInt(out, (int) ((list.getIndentationLeft() + list.getSymbolIndent()) * TWIPSFACTOR));
        out.write(escape);
        out.write(rightIndent);
        writeInt(out, (int) (list.getIndentationRight() * TWIPSFACTOR));
        out.write(escape);
        out.write(fontSize);
        writeInt(out, 20);
        out.write(escape);
        out.write(listBegin);
        writeInt(out, currentListID);
        if (listLevel > 0) {
            out.write(escape);
            out.write(listCurrentLevel);
            writeInt(out, listLevel);
        }
        out.write(openGroup);
        ListIterator listItems = list.getItems().listIterator();
        Element listElem;
        int count = 1;
        while (listItems.hasNext()) {
            listElem = (Element) listItems.next();
            if (listElem.type() == Element.CHUNK) {
                listElem = new ListItem((Chunk) listElem);
            }
            if (listElem.type() == Element.LISTITEM) {
                out.write(openGroup);
                out.write(escape);
                out.write(listTextOld);
                out.write(escape);
                out.write(paragraphDefaults);
                out.write(escape);
                out.write(fontNumber);
                if (list.isNumbered()) {
                    writeInt(out, addFont(new Font(Font.TIMES_ROMAN, Font.NORMAL, 10, new Color(0, 0, 0))));
                } else {
                    writeInt(out, fontNr);
                }
                out.write(escape);
                out.write(firstIndent);
                writeInt(out, (int) (list.getIndentationLeft() * TWIPSFACTOR * -1));
                out.write(escape);
                out.write(listIndent);
                writeInt(out, (int) ((list.getIndentationLeft() + list.getSymbolIndent()) * TWIPSFACTOR));
                out.write(escape);
                out.write(rightIndent);
                writeInt(out, (int) (list.getIndentationRight() * TWIPSFACTOR));
                out.write(delimiter);
                if (list.isNumbered()) {
                    writeInt(out, count);
                    out.write(".".getBytes());
                } else {
                    out.write(escape);
                    out.write(listBulletOld);
                }
                out.write(escape);
                out.write(tab);
                out.write(closeGroup);
                writeListElement((ListItem) listElem, out);
                count++;
            } else if (listElem.type() == Element.LIST) {
                listLevel++;
                writeList((com.lowagie.text.List) listElem, out);
                listLevel--;
                out.write(escape);
                out.write(paragraphDefaults);
                out.write(escape);
                out.write(alignLeft);
                out.write(escape);
                out.write(firstIndent);
                writeInt(out, (int) (list.getIndentationLeft() * TWIPSFACTOR * -1));
                out.write(escape);
                out.write(listIndent);
                writeInt(out, (int) ((list.getIndentationLeft() + list.getSymbolIndent()) * TWIPSFACTOR));
                out.write(escape);
                out.write(rightIndent);
                writeInt(out, (int) (list.getIndentationRight() * TWIPSFACTOR));
                out.write(escape);
                out.write(fontSize);
                writeInt(out, 20);
                out.write(escape);
                out.write(listBegin);
                writeInt(out, currentListID);
                if (listLevel > 0) {
                    out.write(escape);
                    out.write(listCurrentLevel);
                    writeInt(out, listLevel);
                }
            }
            out.write((byte) '\n');
        }
        out.write(closeGroup);
        if (listLevel == 0) {
            int i = getRandomInt();
            listtable.write(escape);
            listtable.write(listID);
            writeInt(listtable, i);
            listtable.write(closeGroup);
            listtable.write((byte) '\n');
            listoverride.write(openGroup);
            listoverride.write(escape);
            listoverride.write(listOverride);
            listoverride.write(escape);
            listoverride.write(listID);
            writeInt(listoverride, i);
            listoverride.write(escape);
            listoverride.write(listOverrideCount);
            writeInt(listoverride, 0);
            listoverride.write(escape);
            listoverride.write(listBegin);
            writeInt(listoverride, currentListID);
            currentListID++;
            listoverride.write(closeGroup);
            listoverride.write((byte) '\n');
        }
        out.write(escape);
        out.write(paragraphDefaults);
    }

    
    private void writeTable(Table table, ByteArrayOutputStream out) throws IOException, DocumentException {
        inTable = true;
        table.complete();
        RtfTable rtfTable = new RtfTable(this);
        rtfTable.importTable(table, pageWidth - marginLeft - marginRight);
        rtfTable.writeTable(out);
        inTable = false;
    }


    
    private void writeImage(Image image, ByteArrayOutputStream out) throws IOException, DocumentException {
        int type = image.getOriginalType();
        if (!(type == Image.ORIGINAL_JPEG || type == Image.ORIGINAL_BMP
            || type == Image.ORIGINAL_PNG || type == Image.ORIGINAL_WMF))
            throw new DocumentException("Only BMP, PNG, WMF and JPEG images are supported by the RTF Writer");
        switch (image.getAlignment()) {
            case Element.ALIGN_LEFT:
                out.write(escape);
                out.write(alignLeft);
                break;
            case Element.ALIGN_RIGHT:
                out.write(escape);
                out.write(alignRight);
                break;
            case Element.ALIGN_CENTER:
                out.write(escape);
                out.write(alignCenter);
                break;
            case Element.ALIGN_JUSTIFIED:
                out.write(escape);
                out.write(alignJustify);
                break;
        }
        out.write(openGroup);
        out.write(extendedEscape);
        out.write(pictureGroup);
        out.write(openGroup);
        out.write(escape);
        out.write(picture);
        out.write(escape);
        switch (type) {
            case Image.ORIGINAL_JPEG:
                out.write(pictureJPEG);
                break;
            case Image.ORIGINAL_PNG:
                out.write(picturePNG);
                break;
            case Image.ORIGINAL_WMF:
            case Image.ORIGINAL_BMP:
                out.write(pictureWMF);
                break;
        }
        out.write(escape);
        out.write(pictureWidth);
        writeInt(out, (int) (image.getPlainWidth() * TWIPSFACTOR));
        out.write(escape);
        out.write(pictureHeight);
        writeInt(out, (int) (image.getPlainHeight() * TWIPSFACTOR));












        if (image.getWidth() > 0) {
            out.write(escape);
            out.write(pictureScaleX);
            writeInt(out, (int) (100 / image.getWidth() * image.getPlainWidth()));
        }
        if (image.getHeight() > 0) {
            out.write(escape);
            out.write(pictureScaleY);
            writeInt(out, (int) (100 / image.getHeight() * image.getPlainHeight()));
        }
        out.write(delimiter);
        InputStream imgIn;
        if (type == Image.ORIGINAL_BMP) {
            imgIn = new ByteArrayInputStream(MetaDo.wrapBMP(image));
        }
        else {
            if (image.getOriginalData() == null) {
                imgIn = image.getUrl().openStream();
            } else {
                imgIn = new ByteArrayInputStream(image.getOriginalData());
            }
            if (type == Image.ORIGINAL_WMF) { 
                long skipLength = 22;
                while(skipLength > 0) {
                    skipLength = skipLength - imgIn.skip(skipLength);
                }
            }
        }
        int buffer = -1;
        int count = 0;
        out.write((byte) '\n');
        while ((buffer = imgIn.read()) != -1) {
            String helperStr = Integer.toHexString(buffer);
            if (helperStr.length() < 2) helperStr = "0" + helperStr;
            out.write(helperStr.getBytes());
            count++;
            if (count == 64) {
                out.write((byte) '\n');
                count = 0;
            }
        }
        imgIn.close();
        out.write(closeGroup);
        out.write(closeGroup);
        out.write((byte) '\n');
    }

    
    private void writeAnnotation(Annotation annotationElement, ByteArrayOutputStream out) throws IOException {
        int id = getRandomInt();
        out.write(openGroup);
        out.write(extendedEscape);
        out.write(annotationID);
        out.write(delimiter);
        writeInt(out, id);
        out.write(closeGroup);
        out.write(openGroup);
        out.write(extendedEscape);
        out.write(annotationAuthor);
        out.write(delimiter);
        out.write(annotationElement.title().getBytes());
        out.write(closeGroup);
        out.write(openGroup);
        out.write(extendedEscape);
        out.write(annotation);
        out.write(escape);
        out.write(paragraphDefaults);
        out.write(delimiter);
        out.write(annotationElement.content().getBytes());
        out.write(closeGroup);
    }

    
    private void writeMeta(byte[] metaName, Meta meta) throws IOException {
        info.write(openGroup);
        try {
            info.write(escape);
            info.write(metaName);
            info.write(delimiter);
            if (meta.type() == Meta.CREATIONDATE) {
                writeFormatedDateTime(meta.getContent());
            } else {
                info.write(meta.getContent().getBytes());
            }
        } finally {
            info.write(closeGroup);
        }
    }

    
    private void writeFormatedDateTime(String date) throws IOException {
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat sdf = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy");
        ParsePosition pp = new ParsePosition(0);
        Date d = sdf.parse(date, pp);
        if (d == null) {
            d = new Date();
        }
        cal.setTime(d);
        info.write(escape);
        info.write(year);
        writeInt(info, cal.get(Calendar.YEAR));
        info.write(escape);
        info.write(month);
        writeInt(info, cal.get(Calendar.MONTH));
        info.write(escape);
        info.write(day);
        writeInt(info, cal.get(Calendar.DAY_OF_MONTH));
        info.write(escape);
        info.write(hour);
        writeInt(info, cal.get(Calendar.HOUR_OF_DAY));
        info.write(escape);
        info.write(minute);
        writeInt(info, cal.get(Calendar.MINUTE));
        info.write(escape);
        info.write(second);
        writeInt(info, cal.get(Calendar.SECOND));
    }

    
    protected int addFont(Font newFont) {
        int fn = -1;

        for (int i = 0; i < fontList.size(); i++) {
            if (newFont.getFamilyname().equals(((Font) fontList.get(i)).getFamilyname())) {
                fn = i;
            }
        }
        if (fn == -1) {
            fontList.add(newFont);
            return fontList.size() - 1;
        }
        return fn;
    }

    
    protected int addColor(Color newColor) {
        int cn = 0;
        if (newColor == null) {
            return cn;
        }
        cn = colorList.indexOf(newColor);
        if (cn == -1) {
            colorList.add(newColor);
            return colorList.size() - 1;
        }
        return cn;
    }

    
    private boolean writeDocument() {
        try {
            writeDocumentIntro();
            writeFontList();
            os.write((byte) '\n');
            writeColorList();
            os.write((byte) '\n');
            writeList();
            os.write((byte) '\n');
            writeInfoGroup();
            os.write((byte) '\n');
            writeDocumentFormat();
            os.write((byte) '\n');
            ByteArrayOutputStream hf = new ByteArrayOutputStream();
            writeSectionDefaults(hf);
            hf.writeTo(os);
            content.writeTo(os);
            os.write(closeGroup);
            return true;
        } catch (IOException e) {
            System.err.println(e.getMessage());
            return false;
        }

    }

    
    private void writeDocumentIntro() throws IOException {
        os.write(openGroup);
        os.write(escape);
        os.write(docBegin);
        os.write(escape);
        os.write(ansi);
        os.write(escape);
        os.write(ansiCodepage);
        writeInt(os, 1252);
        os.write((byte)'\n');
        os.write(escape);
        os.write(defaultFont);
        writeInt(os, 0);
    }

    
    private void writeFontList() throws IOException {
        Font fnt;

        os.write(openGroup);
        os.write(escape);
        os.write(fontTable);
        for (int i = 0; i < fontList.size(); i++) {
            fnt = (Font) fontList.get(i);
            os.write(openGroup);
            os.write(escape);
            os.write(fontNumber);
            writeInt(os, i);
            os.write(escape);
            switch (Font.getFamilyIndex(fnt.getFamilyname())) {
                case Font.COURIER:
                    os.write(fontModern);
                    os.write(escape);
                    os.write(fontCharset);
                    writeInt(os, 0);
                    os.write(delimiter);
                    os.write(fontCourier);
                    break;
                case Font.HELVETICA:
                    os.write(fontSwiss);
                    os.write(escape);
                    os.write(fontCharset);
                    writeInt(os, 0);
                    os.write(delimiter);
                    os.write(fontArial);
                    break;
                case Font.SYMBOL:
                    os.write(fontRoman);
                    os.write(escape);
                    os.write(fontCharset);
                    writeInt(os, 2);
                    os.write(delimiter);
                    os.write(fontSymbol);
                    break;
                case Font.TIMES_ROMAN:
                    os.write(fontRoman);
                    os.write(escape);
                    os.write(fontCharset);
                    writeInt(os, 0);
                    os.write(delimiter);
                    os.write(fontTimesNewRoman);
                    break;
                case Font.ZAPFDINGBATS:
                    os.write(fontTech);
                    os.write(escape);
                    os.write(fontCharset);
                    writeInt(os, 0);
                    os.write(delimiter);
                    os.write(fontWindings);
                    break;
                default:
                    os.write(fontRoman);
                    os.write(escape);
                    os.write(fontCharset);
                    writeInt(os, 0);
                    os.write(delimiter);
                    os.write(filterSpecialChar(fnt.getFamilyname(), true).getBytes());
            }
            os.write(commaDelimiter);
            os.write(closeGroup);
        }
        os.write(closeGroup);
    }

    
    private void writeColorList() throws IOException {
        Color color = null;

        os.write(openGroup);
        os.write(escape);
        os.write(colorTable);
        for (int i = 0; i < colorList.size(); i++) {
            color = (Color) colorList.get(i);
            os.write(escape);
            os.write(colorRed);
            writeInt(os, color.getRed());
            os.write(escape);
            os.write(colorGreen);
            writeInt(os, color.getGreen());
            os.write(escape);
            os.write(colorBlue);
            writeInt(os, color.getBlue());
            os.write(commaDelimiter);
        }
        os.write(closeGroup);
    }

    
    private void writeInfoGroup() throws IOException {
        os.write(openGroup);
        os.write(escape);
        os.write(infoBegin);
        info.writeTo(os);
        os.write(closeGroup);
    }

    
    private void writeList() throws IOException {
        listtable.write(closeGroup);
        listoverride.write(closeGroup);
        listtable.writeTo(os);
        os.write((byte) '\n');
        listoverride.writeTo(os);
    }

    
    public final static void writeInt(OutputStream out, int i) throws IOException {
        out.write(Integer.toString(i).getBytes());
    }

    
    private int getRandomInt() {
        boolean ok = false;
        Integer newInt = null;
        Integer oldInt = null;
        while (!ok) {
            newInt = new Integer((int) (Math.random() * Integer.MAX_VALUE));
            ok = true;
            for (int i = 0; i < listIds.size(); i++) {
                oldInt = (Integer) listIds.get(i);
                if (oldInt.equals(newInt)) {
                    ok = true;
                }
            }
        }
        listIds.add(newInt);
        return newInt.intValue();
    }

    
    public void writeHeadersFooters(ByteArrayOutputStream os) throws IOException {
        if (this.footer instanceof RtfHeaderFooters) {
            RtfHeaderFooters rtfHf = (RtfHeaderFooters) this.footer;
            HeaderFooter hf = rtfHf.get(RtfHeaderFooters.ALL_PAGES);
            if (hf != null) {
                writeHeaderFooter(hf, footerBegin, os);
            }
            hf = rtfHf.get(RtfHeaderFooters.LEFT_PAGES);
            if (hf != null) {
                writeHeaderFooter(hf, footerlBegin, os);
            }
            hf = rtfHf.get(RtfHeaderFooters.RIGHT_PAGES);
            if (hf != null) {
                writeHeaderFooter(hf, footerrBegin, os);
            }
            hf = rtfHf.get(RtfHeaderFooters.FIRST_PAGE);
            if (hf != null) {
                writeHeaderFooter(hf, footerfBegin, os);
            }
        } else {
            writeHeaderFooter(this.footer, footerBegin, os);
        }
        if (this.header instanceof RtfHeaderFooters) {
            RtfHeaderFooters rtfHf = (RtfHeaderFooters) this.header;
            HeaderFooter hf = rtfHf.get(RtfHeaderFooters.ALL_PAGES);
            if (hf != null) {
                writeHeaderFooter(hf, headerBegin, os);
            }
            hf = rtfHf.get(RtfHeaderFooters.LEFT_PAGES);
            if (hf != null) {
                writeHeaderFooter(hf, headerlBegin, os);
            }
            hf = rtfHf.get(RtfHeaderFooters.RIGHT_PAGES);
            if (hf != null) {
                writeHeaderFooter(hf, headerrBegin, os);
            }
            hf = rtfHf.get(RtfHeaderFooters.FIRST_PAGE);
            if (hf != null) {
                writeHeaderFooter(hf, headerfBegin, os);
            }
        } else {
            writeHeaderFooter(this.header, headerBegin, os);
        }
    }

    
    private void writeHeaderFooter(HeaderFooter headerFooter, byte[] hfType, ByteArrayOutputStream target) throws IOException {
        inHeaderFooter = true;
        try {
            target.write(openGroup);
            target.write(escape);
            target.write(hfType);
            target.write(delimiter);
            if (headerFooter != null) {
                if (headerFooter instanceof RtfHeaderFooter && ((RtfHeaderFooter) headerFooter).content() != null) {
                    this.addElement(((RtfHeaderFooter) headerFooter).content(), target);
                } else {
                    Paragraph par = new Paragraph();
                    par.setAlignment(headerFooter.alignment());
                    if (headerFooter.getBefore() != null) {
                        par.add(headerFooter.getBefore());
                    }
                    if (headerFooter.isNumbered()) {
                        par.add(new RtfPageNumber("", headerFooter.getBefore().getFont()));
                    }
                    if (headerFooter.getAfter() != null) {
                        par.add(headerFooter.getAfter());
                    }
                    this.addElement(par, target);
                }
            }
            target.write(closeGroup);
        } catch (DocumentException e) {
            throw new IOException("DocumentException - " + e.getMessage());
        }
        inHeaderFooter = false;
    }

    
    private void writeDocumentFormat() throws IOException {

        os.write(escape);
        os.write(rtfPaperWidth);
        writeInt(os, pageWidth);
        os.write(escape);
        os.write(rtfPaperHeight);
        writeInt(os, pageHeight);
        os.write(escape);
        os.write(rtfMarginLeft);
        writeInt(os, marginLeft);
        os.write(escape);
        os.write(rtfMarginRight);
        writeInt(os, marginRight);
        os.write(escape);
        os.write(rtfMarginTop);
        writeInt(os, marginTop);
        os.write(escape);
        os.write(rtfMarginBottom);
        writeInt(os, marginBottom);

    }

    
    private void initDefaults() {
        fontList.clear();
        colorList.clear();
        info = new ByteArrayOutputStream();
        content = new ByteArrayOutputStream();
        listtable = new ByteArrayOutputStream();
        listoverride = new ByteArrayOutputStream();
        document.addProducer();
        document.addCreationDate();
        addFont(new Font(Font.TIMES_ROMAN, 10, Font.NORMAL));
        addColor(new Color(0, 0, 0));
        addColor(new Color(255, 255, 255));
        listIds = new ArrayList();
        try {
            listtable.write(openGroup);
            listtable.write(extendedEscape);
            listtable.write(listtableGroup);
            listtable.write((byte) '\n');
            listoverride.write(openGroup);
            listoverride.write(extendedEscape);
            listoverride.write(listoverridetableGroup);
            listoverride.write((byte) '\n');
        } catch (IOException e) {
            System.err.println("InitDefaultsError" + e);
        }
    }

    
    private void writeSectionDefaults(ByteArrayOutputStream out) throws IOException {
        if (header instanceof RtfHeaderFooters || footer instanceof RtfHeaderFooters) {
            RtfHeaderFooters rtfHeader = (RtfHeaderFooters) header;
            RtfHeaderFooters rtfFooter = (RtfHeaderFooters) footer;
            if ((rtfHeader != null && (rtfHeader.get(RtfHeaderFooters.LEFT_PAGES) != null || rtfHeader.get(RtfHeaderFooters.RIGHT_PAGES) != null)) || (rtfFooter != null && (rtfFooter.get(RtfHeaderFooters.LEFT_PAGES) != null || rtfFooter.get(RtfHeaderFooters.RIGHT_PAGES) != null))) {
                out.write(escape);
                out.write(facingPages);
            }
        }
        if (hasTitlePage) {
            out.write(escape);
            out.write(titlePage);
        }
        writeHeadersFooters(out);
        if (landscape) {
            
            
            out.write(escape);
            out.write(landscapeTag2);
            out.write(escape);
            out.write(sectionPageWidth);
            writeInt(out, pageWidth);
            out.write(escape);
            out.write(sectionPageHeight);
            writeInt(out, pageHeight);
        } else {
            out.write(escape);
            out.write(sectionPageWidth);
            writeInt(out, pageWidth);
            out.write(escape);
            out.write(sectionPageHeight);
            writeInt(out, pageHeight);
        }
    }

    
    private boolean parseFormat(Rectangle pageSize, boolean rotate) {
        if (rotate) {
            pageSize = pageSize.rotate();
        }
        if (rectEquals(pageSize, PageSize.A3)) {
            pageWidth = 16837;
            pageHeight = 23811;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.A4)) {
            pageWidth = 11907;
            pageHeight = 16840;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.A5)) {
            pageWidth = 8391;
            pageHeight = 11907;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.A6)) {
            pageWidth = 5959;
            pageHeight = 8420;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.B4)) {
            pageWidth = 14570;
            pageHeight = 20636;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.B5)) {
            pageWidth = 10319;
            pageHeight = 14572;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.HALFLETTER)) {
            pageWidth = 7927;
            pageHeight = 12247;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.LETTER)) {
            pageWidth = 12242;
            pageHeight = 15842;
            landscape = rotate;
            return true;
        }
        if (rectEquals(pageSize, PageSize.LEGAL)) {
            pageWidth = 12252;
            pageHeight = 20163;
            landscape = rotate;
            return true;
        }
        if (!rotate && parseFormat(pageSize, true)) {
            int x = pageWidth;
            pageWidth = pageHeight;
            pageHeight = x;
            return true;
        }
        return false;
    }

    
    private boolean rectEquals(Rectangle rect1, Rectangle rect2) {
        return (rect1.getWidth() == rect2.getWidth()) && (rect1.getHeight() == rect2.getHeight());
    }

    
    public boolean writingHeaderFooter() {
        return inHeaderFooter;
    }

    
    public final static String filterSpecialChar(String str, boolean useHex) {
        int length = str.length();
        int z = (int) 'z';
        StringBuffer ret = new StringBuffer(length);
        for (int i = 0; i < length; i++) {
            char ch = str.charAt(i);

            if (ch == '\\') {
                ret.append("\\\\");
            } else if (ch == '\n') {
                ret.append("\\par ");
            } else if (((int) ch) > z) {
                if(useHex) {
                    ret.append("\\\'").append(Long.toHexString((long) ch));
                } else {
                ret.append("\\u").append((long) ch).append('?');
                }
            } else {
                ret.append(ch);
            }
        }
        String s = ret.toString();
        if(s.indexOf("$newpage$") >= 0) {
            String before = s.substring(0, s.indexOf("$newpage$"));
            String after = s.substring(s.indexOf("$newpage$") + 9);
            ret = new StringBuffer(before);
            ret.append("\\page\\par ");
            ret.append(after);
            return ret.toString();
        }
        return s;
    }

    private void addHeaderFooterFontColor(HeaderFooter hf) {
        if(hf instanceof RtfHeaderFooter) {
            RtfHeaderFooter rhf = (RtfHeaderFooter) hf;
            if(rhf.content() instanceof Chunk) {
                addFont(((Chunk) rhf.content()).getFont());
                addColor(((Chunk) rhf.content()).getFont().getColor());
            } else if(rhf.content() instanceof Phrase) {
                addFont(((Phrase) rhf.content()).getFont());
                addColor(((Phrase) rhf.content()).getFont().getColor());
            }
        }
        if(hf.getBefore() != null) {
            addFont(hf.getBefore().getFont());
            addColor(hf.getBefore().getFont().getColor());
        }
        if(hf.getAfter() != null) {
            addFont(hf.getAfter().getFont());
            addColor(hf.getAfter().getFont().getColor());
        }
    }

    private void processHeaderFooter(HeaderFooter hf) {
        if(hf != null) {
            if(hf instanceof RtfHeaderFooters) {
                RtfHeaderFooters rhf = (RtfHeaderFooters) hf;
                if(rhf.get(RtfHeaderFooters.ALL_PAGES) != null) {
                    addHeaderFooterFontColor(rhf.get(RtfHeaderFooters.ALL_PAGES));
                }
                if(rhf.get(RtfHeaderFooters.LEFT_PAGES) != null) {
                    addHeaderFooterFontColor(rhf.get(RtfHeaderFooters.LEFT_PAGES));
                }
                if(rhf.get(RtfHeaderFooters.RIGHT_PAGES) != null) {
                    addHeaderFooterFontColor(rhf.get(RtfHeaderFooters.RIGHT_PAGES));
                }
                if(rhf.get(RtfHeaderFooters.FIRST_PAGE) != null) {
                    addHeaderFooterFontColor(rhf.get(RtfHeaderFooters.FIRST_PAGE));
                }
            } else {
                addHeaderFooterFontColor(hf);
            }
        }
    }
    
    
    public boolean setMarginMirroring(boolean MarginMirroring) {
        return false;
    }
    
}

