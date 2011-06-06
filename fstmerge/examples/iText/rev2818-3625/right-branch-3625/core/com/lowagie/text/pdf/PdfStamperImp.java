
package com.lowagie.text.pdf;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;

import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.collection.PdfCollection;
import com.lowagie.text.pdf.interfaces.PdfViewerPreferences;
import com.lowagie.text.pdf.internal.PdfViewerPreferencesImp;
import com.lowagie.text.xml.xmp.XmpReader;

class PdfStamperImp extends PdfWriter {
    HashMap<PdfReader, IntHashtable> readers2intrefs = new HashMap<PdfReader, IntHashtable>();
    HashMap<PdfReader, RandomAccessFileOrArray> readers2file = new HashMap<PdfReader, RandomAccessFileOrArray>();
    RandomAccessFileOrArray file;
    PdfReader reader;
    IntHashtable myXref = new IntHashtable();
    
    HashMap<PdfDictionary, PageStamp> pagesToContent = new HashMap<PdfDictionary, PageStamp>();
    boolean closed = false;
    
    private boolean rotateContents = true;
    protected AcroFields acroFields;
    protected boolean flat = false;
    protected boolean flatFreeText = false;
    protected int namePtr[] = {0};
    protected HashSet<String> partialFlattening = new HashSet<String>();
    protected boolean useVp = false;
    protected PdfViewerPreferencesImp viewerPreferences = new PdfViewerPreferencesImp();
    protected HashSet<PdfTemplate> fieldTemplates = new HashSet<PdfTemplate>();
    protected boolean fieldsAdded = false;
    protected int sigFlags = 0;
    protected boolean append;
    protected IntHashtable marked;
    protected int initialXrefSize;
    protected PdfAction openAction;
    
    
    PdfStamperImp(PdfReader reader, OutputStream os, char pdfVersion, boolean append) throws DocumentException, IOException {
        super(new PdfDocument(), os);
        if (!reader.isOpenedWithFullPermissions())
            throw new IllegalArgumentException("PdfReader not opened with owner password");
        if (reader.isTampered())
            throw new DocumentException("The original document was reused. Read it again from file.");
        reader.setTampered(true);
        this.reader = reader;
        file = reader.getSafeFile();
        this.append = append;
        if (append) {
            if (reader.isRebuilt())
                throw new DocumentException("Append mode requires a document without errors even if recovery was possible.");
            if (reader.isEncrypted())
                crypto = new PdfEncryption(reader.getDecrypt());
            pdf_version.setAppendmode(true);
            file.reOpen();
            byte buf[] = new byte[8192];
            int n;
            while ((n = file.read(buf)) > 0)
                this.os.write(buf, 0, n);
            file.close();
            prevxref = reader.getLastXref();
            reader.setAppendable(true);
        }
        else {
            if (pdfVersion == 0)
                super.setPdfVersion(reader.getPdfVersion());
            else
                super.setPdfVersion(pdfVersion);
        }
        super.open();
        pdf.addWriter(this);
        if (append) {
            body.setRefnum(reader.getXrefSize());
            marked = new IntHashtable();
            if (reader.isNewXrefType())
                fullCompression = true;
            if (reader.isHybridXref())
                fullCompression = false;
        }
        initialXrefSize = reader.getXrefSize();
    }
    
    void close(HashMap<String, String> moreInfo) throws IOException {
        if (closed)
            return;
        if (useVp) {
            reader.setViewerPreferences(viewerPreferences);
            markUsed(reader.getTrailer().get(PdfName.ROOT));
        }
        if (flat)
            flatFields();
        if (flatFreeText)
            flatFreeTextFields();
        addFieldResources();
        PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(reader.getCatalog().get(PdfName.ACROFORM), reader.getCatalog());
        if (acroFields != null && acroFields.getXfa().isChanged()) {
            markUsed(acroForm);
            if (!flat)
                acroFields.getXfa().setXfa(this);
        }
        if (sigFlags != 0) {
            if (acroForm != null) {
                acroForm.put(PdfName.SIGFLAGS, new PdfNumber(sigFlags));
                markUsed(acroForm);
            }
        }
        closed = true;
        addSharedObjectsToBody();
        setOutlines();
        setJavaScript();
        addFileAttachments();
        PdfDictionary catalog = reader.getCatalog();            
        if (openAction != null) {
            catalog.put(PdfName.OPENACTION, openAction);
        }
        if (pdf.pageLabels != null)
            catalog.put(PdfName.PAGELABELS, pdf.pageLabels.getDictionary(this));
        byte[] altMetadata = null;
        PdfObject xmpo = PdfReader.getPdfObject(catalog.get(PdfName.METADATA));
        if (xmpo != null && xmpo.isStream()) {
            altMetadata = PdfReader.getStreamBytesRaw((PRStream)xmpo);
            PdfReader.killIndirect(catalog.get(PdfName.METADATA));
        }
        if (xmpMetadata != null) {
            altMetadata = xmpMetadata;
        }
        
        PdfString date = new PdfDate();
        if (altMetadata != null) {
            XmpReader xmpr = new XmpReader(altMetadata);
            xmpr.replace("http://ns.adobe.com/xap/1.0/", "ModifyDate", date.toString());
            xmpr.replace("http://ns.adobe.com/xap/1.0/", "MetadataDate", date.toString());
            PdfStream xmp = new PdfStream(xmpr.serializeDoc());
            xmp.put(PdfName.TYPE, PdfName.METADATA);
            xmp.put(PdfName.SUBTYPE, PdfName.XML);
            if (crypto != null && !crypto.isMetadataEncrypted()) {
                PdfArray ar = new PdfArray();
                ar.add(PdfName.CRYPT);
                xmp.put(PdfName.FILTER, ar);
            }
            if (append && xmpo != null) {
                body.add(xmp, xmpo.getIndRef());
            }
            else {
                catalog.put(PdfName.METADATA, body.add(xmp).getIndirectReference());
                markUsed(catalog);
            }
        }
        if (!documentOCG.isEmpty()) {
            fillOCProperties(false);
            PdfDictionary ocdict = catalog.getAsDict(PdfName.OCPROPERTIES);
            if (ocdict == null) {
                reader.getCatalog().put(PdfName.OCPROPERTIES, OCProperties);
            }
            else {
                ocdict.put(PdfName.OCGS, OCProperties.get(PdfName.OCGS));
                PdfDictionary ddict = ocdict.getAsDict(PdfName.D);
                ddict.put(PdfName.ORDER, OCProperties.getAsDict(PdfName.D).get(PdfName.ORDER));
                ddict.put(PdfName.RBGROUPS, OCProperties.getAsDict(PdfName.D).get(PdfName.RBGROUPS));
                ddict.put(PdfName.OFF, OCProperties.getAsDict(PdfName.D).get(PdfName.OFF));
                ddict.put(PdfName.AS, OCProperties.getAsDict(PdfName.D).get(PdfName.AS));
            }
        }
        PRIndirectReference iInfo = null;
        try {
            file.reOpen();
            alterContents();
            iInfo = (PRIndirectReference)reader.trailer.get(PdfName.INFO);
            int skip = -1;
            if (iInfo != null)
                skip = iInfo.getNumber();
            int rootN = ((PRIndirectReference)reader.trailer.get(PdfName.ROOT)).getNumber();
            if (append) {
                int keys[] = marked.getKeys();
                for (int k = 0; k < keys.length; ++k) {
                    int j = keys[k];
                    PdfObject obj = reader.getPdfObjectRelease(j);
                    if (obj != null && skip != j && j < initialXrefSize) {
                        addToBody(obj, j, j != rootN);
                    }
                }
                for (int k = initialXrefSize; k < reader.getXrefSize(); ++k) {
                    PdfObject obj = reader.getPdfObject(k);
                    if (obj != null) {
                        addToBody(obj, getNewObjectNumber(reader, k, 0));
                    }
                }
            }
            else {
                for (int k = 1; k < reader.getXrefSize(); ++k) {
                    PdfObject obj = reader.getPdfObjectRelease(k);
                    if (obj != null && skip != k) {
                        addToBody(obj, getNewObjectNumber(reader, k, 0), k != rootN);
                    }
                }
            }
        }
        finally {
            try {
                file.close();
            }
            catch (Exception e) {
                
            }
        }
        PdfIndirectReference encryption = null;
        PdfObject fileID = null;
        if (crypto != null) {
            if (append) {
                encryption = reader.getCryptoRef();
            }
            else {
                PdfIndirectObject encryptionObject = addToBody(crypto.getEncryptionDictionary(), false);
                encryption = encryptionObject.getIndirectReference();
            }
            fileID = crypto.getFileID();
        }
        else
            fileID = PdfEncryption.createInfoId(PdfEncryption.createDocumentId());
        PRIndirectReference iRoot = (PRIndirectReference)reader.trailer.get(PdfName.ROOT);
        PdfIndirectReference root = new PdfIndirectReference(0, getNewObjectNumber(reader, iRoot.getNumber(), 0));
        PdfIndirectReference info = null;
        PdfDictionary oldInfo = (PdfDictionary)PdfReader.getPdfObject(iInfo);
        PdfDictionary newInfo = new PdfDictionary();
        if (oldInfo != null) {
            for (PdfName key: oldInfo.getKeys()) {
                PdfObject value = PdfReader.getPdfObject(oldInfo.get(key));
                newInfo.put(key, value);
            }
        }
        if (moreInfo != null) {
            for (Map.Entry<String, String> entry: moreInfo.entrySet()) {
                String key = entry.getKey();
                PdfName keyName = new PdfName(key);
                String value = entry.getValue();
                if (value == null)
                    newInfo.remove(keyName);
                else
                    newInfo.put(keyName, new PdfString(value, PdfObject.TEXT_UNICODE));
            }
        }
        newInfo.put(PdfName.MODDATE, date);
        if (append) {
            if (iInfo == null)
                info = addToBody(newInfo, false).getIndirectReference();
            else
                info = addToBody(newInfo, iInfo.getNumber(), false).getIndirectReference();
        }
        else {
            info = addToBody(newInfo, false).getIndirectReference();
        }
        
        body.writeCrossReferenceTable(os, root, info, encryption, fileID, prevxref);
        if (fullCompression) {
            os.write(getISOBytes("startxref\n"));
            os.write(getISOBytes(String.valueOf(body.offset())));
            os.write(getISOBytes("\n%%EOF\n"));
        }
        else {
            PdfTrailer trailer = new PdfTrailer(body.size(),
            body.offset(),
            root,
            info,
            encryption,
            fileID, prevxref);
            trailer.toPdf(this, os);
        }
        os.flush();
        if (isCloseStream())
            os.close();
        reader.close();
    }
    
    void applyRotation(PdfDictionary pageN, ByteBuffer out) {
        if (!rotateContents)
            return;
        Rectangle page = reader.getPageSizeWithRotation(pageN);
        int rotation = page.getRotation();
        switch (rotation) {
            case 90:
                out.append(PdfContents.ROTATE90);
                out.append(page.getTop());
                out.append(' ').append('0').append(PdfContents.ROTATEFINAL);
                break;
            case 180:
                out.append(PdfContents.ROTATE180);
                out.append(page.getRight());
                out.append(' ');
                out.append(page.getTop());
                out.append(PdfContents.ROTATEFINAL);
                break;
            case 270:
                out.append(PdfContents.ROTATE270);
                out.append('0').append(' ');
                out.append(page.getRight());
                out.append(PdfContents.ROTATEFINAL);
                break;
        }
    }
    
    void alterContents() throws IOException {
        for (PageStamp ps: pagesToContent.values()) {
            PdfDictionary pageN = ps.pageN;
            markUsed(pageN);
            PdfArray ar = null;
            PdfObject content = PdfReader.getPdfObject(pageN.get(PdfName.CONTENTS), pageN);
            if (content == null) {
                ar = new PdfArray();
                pageN.put(PdfName.CONTENTS, ar);
            }
            else if (content.isArray()) {
                ar = (PdfArray)content;
                markUsed(ar);
            }
            else if (content.isStream()) {
                ar = new PdfArray();
                ar.add(pageN.get(PdfName.CONTENTS));
                pageN.put(PdfName.CONTENTS, ar);
            }
            else {
                ar = new PdfArray();
                pageN.put(PdfName.CONTENTS, ar);
            }
            ByteBuffer out = new ByteBuffer();
            if (ps.under != null) {
                out.append(PdfContents.SAVESTATE);
                applyRotation(pageN, out);
                out.append(ps.under.getInternalBuffer());
                out.append(PdfContents.RESTORESTATE);
            }
            if (ps.over != null)
                out.append(PdfContents.SAVESTATE);
            PdfStream stream = new PdfStream(out.toByteArray());
            stream.flateCompress(compressionLevel);
            ar.addFirst(addToBody(stream).getIndirectReference());
            out.reset();
            if (ps.over != null) {
                out.append(' ');
                out.append(PdfContents.RESTORESTATE);
                ByteBuffer buf = ps.over.getInternalBuffer();
                out.append(buf.getBuffer(), 0, ps.replacePoint);
                out.append(PdfContents.SAVESTATE);
                applyRotation(pageN, out);
                out.append(buf.getBuffer(), ps.replacePoint, buf.size() - ps.replacePoint);
                out.append(PdfContents.RESTORESTATE);
                stream = new PdfStream(out.toByteArray());
                stream.flateCompress(compressionLevel);
                ar.add(addToBody(stream).getIndirectReference());
            }
            alterResources(ps);
        }
    }

    void alterResources(PageStamp ps) {
        ps.pageN.put(PdfName.RESOURCES, ps.pageResources.getResources());
    }
    
    protected int getNewObjectNumber(PdfReader reader, int number, int generation) {
        IntHashtable ref = readers2intrefs.get(reader);
        if (ref != null) {
            int n = ref.get(number);
            if (n == 0) {
                n = getIndirectReferenceNumber();
                ref.put(number, n);
            }
            return n;
        }
        if (currentPdfReaderInstance == null) {
            if (append && number < initialXrefSize)
                return number;
            int n = myXref.get(number);
            if (n == 0) {
                n = getIndirectReferenceNumber();
                myXref.put(number, n);
            }
            return n;
        }
        else
            return currentPdfReaderInstance.getNewObjectNumber(number, generation);
    }
    
    RandomAccessFileOrArray getReaderFile(PdfReader reader) {
        if (readers2intrefs.containsKey(reader)) {
            RandomAccessFileOrArray raf = readers2file.get(reader);
            if (raf != null)
                return raf;
            return reader.getSafeFile();
        }
        if (currentPdfReaderInstance == null)
            return file;
        else
            return currentPdfReaderInstance.getReaderFile();
    }
    
    
    public void registerReader(PdfReader reader, boolean openFile) throws IOException {
        if (readers2intrefs.containsKey(reader))
            return;
        readers2intrefs.put(reader, new IntHashtable());
        if (openFile) {
            RandomAccessFileOrArray raf = reader.getSafeFile();
            readers2file.put(reader, raf);
            raf.reOpen();
        }
    }
    
    
    public void unRegisterReader(PdfReader reader) {
        if (!readers2intrefs.containsKey(reader))
            return;
        readers2intrefs.remove(reader);
        RandomAccessFileOrArray raf = readers2file.get(reader);
        if (raf == null)
            return;
        readers2file.remove(reader);
        try{raf.close();}catch(Exception e){}
    }

    static void findAllObjects(PdfReader reader, PdfObject obj, IntHashtable hits) {
        if (obj == null)
            return;
        switch (obj.type()) {
            case PdfObject.INDIRECT:
                PRIndirectReference iref = (PRIndirectReference)obj;
                if (reader != iref.getReader())
                    return;
                if (hits.containsKey(iref.getNumber()))
                    return;
                hits.put(iref.getNumber(), 1);
                findAllObjects(reader, PdfReader.getPdfObject(obj), hits);
                return;
            case PdfObject.ARRAY:
                ArrayList<PdfObject> lst = ((PdfArray)obj).getArrayList();
                for (int k = 0; k < lst.size(); ++k) {
                    findAllObjects(reader, lst.get(k), hits);
                }
                return;
            case PdfObject.DICTIONARY:
            case PdfObject.STREAM:
                PdfDictionary dic = (PdfDictionary)obj;
                for (PdfName name: dic.getKeys()) {
                    findAllObjects(reader, dic.get(name), hits);
                }
                return;
        }
    }
    
    
    public void addComments(FdfReader fdf) throws IOException{
        if (readers2intrefs.containsKey(fdf))
            return;
        PdfDictionary catalog = fdf.getCatalog();
        catalog = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.FDF));
        if (catalog == null)
            return;
        PdfArray annots = (PdfArray)PdfReader.getPdfObject(catalog.get(PdfName.ANNOTS));
        if (annots == null || annots.size() == 0)
            return;
        registerReader(fdf, false);
        IntHashtable hits = new IntHashtable();
        HashMap<String, PdfObject> irt = new HashMap<String, PdfObject>();
        ArrayList<PdfObject> an = new ArrayList<PdfObject>();
        ArrayList<PdfObject> ar = annots.getArrayList();
        for (int k = 0; k < ar.size(); ++k) {
            PdfObject obj = ar.get(k);
            PdfDictionary annot = (PdfDictionary)PdfReader.getPdfObject(obj);
            PdfNumber page = (PdfNumber)PdfReader.getPdfObject(annot.get(PdfName.PAGE));
            if (page == null || page.intValue() >= reader.getNumberOfPages())
                continue;
            findAllObjects(fdf, obj, hits);
            an.add(obj);
            if (obj.type() == PdfObject.INDIRECT) {
                PdfObject nm = PdfReader.getPdfObject(annot.get(PdfName.NM));
                if (nm != null && nm.type() == PdfObject.STRING)
                    irt.put(nm.toString(), obj);
            }
        }
        int arhits[] = hits.getKeys();
        for (int k = 0; k < arhits.length; ++k) {
            int n = arhits[k];
            PdfObject obj = fdf.getPdfObject(n);
            if (obj.type() == PdfObject.DICTIONARY) {
                PdfObject str = PdfReader.getPdfObject(((PdfDictionary)obj).get(PdfName.IRT));
                if (str != null && str.type() == PdfObject.STRING) {
                   PdfObject i = irt.get(str.toString());
                   if (i != null) {
                       PdfDictionary dic2 = new PdfDictionary();
                       dic2.merge((PdfDictionary)obj);
                       dic2.put(PdfName.IRT, i);
                       obj = dic2;
                   }
                }
            }
            addToBody(obj, getNewObjectNumber(fdf, n, 0));
        }
        for (int k = 0; k < an.size(); ++k) {
            PdfObject obj = an.get(k);
            PdfDictionary annot = (PdfDictionary)PdfReader.getPdfObject(obj);
            PdfNumber page = (PdfNumber)PdfReader.getPdfObject(annot.get(PdfName.PAGE));
            PdfDictionary dic = reader.getPageN(page.intValue() + 1);
            PdfArray annotsp = (PdfArray)PdfReader.getPdfObject(dic.get(PdfName.ANNOTS), dic);
            if (annotsp == null) {
                annotsp = new PdfArray();
                dic.put(PdfName.ANNOTS, annotsp);
                markUsed(dic);
            }
            markUsed(annotsp);
            annotsp.add(obj);
        }
    }
    
    PageStamp getPageStamp(int pageNum) {
        PdfDictionary pageN = reader.getPageN(pageNum);
        PageStamp ps = pagesToContent.get(pageN);
        if (ps == null) {
            ps = new PageStamp(this, reader, pageN);
            pagesToContent.put(pageN, ps);
        }
        return ps;
    }
    
    PdfContentByte getUnderContent(int pageNum) {
        if (pageNum < 1 || pageNum > reader.getNumberOfPages())
            return null;
        PageStamp ps = getPageStamp(pageNum);
        if (ps.under == null)
            ps.under = new StampContent(this, ps);
        return ps.under;
    }
    
    PdfContentByte getOverContent(int pageNum) {
        if (pageNum < 1 || pageNum > reader.getNumberOfPages())
            return null;
        PageStamp ps = getPageStamp(pageNum);
        if (ps.over == null)
            ps.over = new StampContent(this, ps);
        return ps.over;
    }
    
    void correctAcroFieldPages(int page) {
        if (acroFields == null)
            return;
        if (page > reader.getNumberOfPages())
            return;
        HashMap<String, AcroFields.Item> fields = acroFields.getFields();
        for (AcroFields.Item item: fields.values()) {
            ArrayList<Integer> pages = item.page;
            for (int k = 0; k < pages.size(); ++k) {
                int p = pages.get(k).intValue();
                if (p >= page)
                    pages.set(k, new Integer(p + 1));
            }
        }
    }
    
    private static void moveRectangle(PdfDictionary dic2, PdfReader r, int pageImported, PdfName key, String name) {
        Rectangle m = r.getBoxSize(pageImported, name);
        if (m == null)
            dic2.remove(key);
        else
            dic2.put(key, new PdfRectangle(m));
    }
    
    void replacePage(PdfReader r, int pageImported, int pageReplaced) {
        PdfDictionary pageN = reader.getPageN(pageReplaced);
        if (pagesToContent.containsKey(pageN))
            throw new IllegalStateException("This page cannot be replaced: new content was already added");
        PdfImportedPage p = getImportedPage(r, pageImported);
        PdfDictionary dic2 = reader.getPageNRelease(pageReplaced);
        dic2.remove(PdfName.RESOURCES);
        dic2.remove(PdfName.CONTENTS);
        moveRectangle(dic2, r, pageImported, PdfName.MEDIABOX, "media");
        moveRectangle(dic2, r, pageImported, PdfName.CROPBOX, "crop");
        moveRectangle(dic2, r, pageImported, PdfName.TRIMBOX, "trim");
        moveRectangle(dic2, r, pageImported, PdfName.ARTBOX, "art");
        moveRectangle(dic2, r, pageImported, PdfName.BLEEDBOX, "bleed");
        dic2.put(PdfName.ROTATE, new PdfNumber(r.getPageRotation(pageImported)));
        PdfContentByte cb = getOverContent(pageReplaced);
        cb.addTemplate(p, 0, 0);
        PageStamp ps = pagesToContent.get(pageN);
        ps.replacePoint = ps.over.getInternalBuffer().size();
    }
    
    void insertPage(int pageNumber, Rectangle mediabox) {
        Rectangle media = new Rectangle(mediabox);
        int rotation = media.getRotation() % 360;
        PdfDictionary page = new PdfDictionary(PdfName.PAGE);
        PdfDictionary resources = new PdfDictionary();
        PdfArray procset = new PdfArray();
        procset.add(PdfName.PDF);
        procset.add(PdfName.TEXT);
        procset.add(PdfName.IMAGEB);
        procset.add(PdfName.IMAGEC);
        procset.add(PdfName.IMAGEI);
        resources.put(PdfName.PROCSET, procset);
        page.put(PdfName.RESOURCES, resources);
        page.put(PdfName.ROTATE, new PdfNumber(rotation));
        page.put(PdfName.MEDIABOX, new PdfRectangle(media, rotation));
        PRIndirectReference pref = reader.addPdfObject(page);
        PdfDictionary parent;
        PRIndirectReference parentRef;
        if (pageNumber > reader.getNumberOfPages()) {
            PdfDictionary lastPage = reader.getPageNRelease(reader.getNumberOfPages());
            parentRef = (PRIndirectReference)lastPage.get(PdfName.PARENT);
            parentRef = new PRIndirectReference(reader, parentRef.getNumber());
            parent = (PdfDictionary)PdfReader.getPdfObject(parentRef);
            PdfArray kids = (PdfArray)PdfReader.getPdfObject(parent.get(PdfName.KIDS), parent);
            kids.add(pref);
            markUsed(kids);
            reader.pageRefs.insertPage(pageNumber, pref);
        }
        else {
            if (pageNumber < 1)
                pageNumber = 1;
            PdfDictionary firstPage = reader.getPageN(pageNumber);
            PRIndirectReference firstPageRef = reader.getPageOrigRef(pageNumber);
            reader.releasePage(pageNumber);
            parentRef = (PRIndirectReference)firstPage.get(PdfName.PARENT);
            parentRef = new PRIndirectReference(reader, parentRef.getNumber());
            parent = (PdfDictionary)PdfReader.getPdfObject(parentRef);
            PdfArray kids = (PdfArray)PdfReader.getPdfObject(parent.get(PdfName.KIDS), parent);
            ArrayList<PdfObject> ar = kids.getArrayList();
            int len = ar.size();
            int num = firstPageRef.getNumber();
            for (int k = 0; k < len; ++k) {
                PRIndirectReference cur = (PRIndirectReference)ar.get(k);
                if (num == cur.getNumber()) {
                    ar.add(k, pref);
                    break;
                }
            }
            if (len == ar.size())
                throw new RuntimeException("Internal inconsistence.");
            markUsed(kids);
            reader.pageRefs.insertPage(pageNumber, pref);
            correctAcroFieldPages(pageNumber);
        }
        page.put(PdfName.PARENT, parentRef);
        while (parent != null) {
            markUsed(parent);
            PdfNumber count = (PdfNumber)PdfReader.getPdfObjectRelease(parent.get(PdfName.COUNT));
            parent.put(PdfName.COUNT, new PdfNumber(count.intValue() + 1));
            parent = (PdfDictionary)PdfReader.getPdfObject(parent.get(PdfName.PARENT));
        }
    }
    
    
    boolean isRotateContents() {
        return this.rotateContents;
    }
    
    
    void setRotateContents(boolean rotateContents) {
        this.rotateContents = rotateContents;
    }
    
    boolean isContentWritten() {
        return body.size() > 1;
    }
    
    AcroFields getAcroFields() {
        if (acroFields == null) {
            acroFields = new AcroFields(reader, this);
        }
        return acroFields;
    }

    void setFormFlattening(boolean flat) {
        this.flat = flat;
    }
    
    void setFreeTextFlattening(boolean flat) {
        this.flatFreeText = flat;
    }
    
    boolean partialFormFlattening(String name) {
        getAcroFields();
        if (acroFields.getXfa().isXfaPresent())
            throw new UnsupportedOperationException("Partial form flattening is not supported with XFA forms.");
        if (!acroFields.getFields().containsKey(name))
            return false;
        partialFlattening.add(name);
        return true;
    }
    
    void flatFields() {
        if (append)
            throw new IllegalArgumentException("Field flattening is not supported in append mode.");
        getAcroFields();
        HashMap<String, AcroFields.Item> fields = acroFields.getFields();
        if (fieldsAdded && partialFlattening.isEmpty()) {
            for (String s: fields.keySet()) {
                partialFlattening.add(s);
            }
        }
        PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(reader.getCatalog().get(PdfName.ACROFORM));
        ArrayList<PdfObject> acroFds = null;
        if (acroForm != null) {
            PdfArray array = (PdfArray)PdfReader.getPdfObject(acroForm.get(PdfName.FIELDS), acroForm);
            if (array != null)
                acroFds = array.getArrayList();
        }
        for (Map.Entry<String, AcroFields.Item> entry: fields.entrySet()) {
            String name = entry.getKey();
            if (!partialFlattening.isEmpty() && !partialFlattening.contains(name))
                continue;
            AcroFields.Item item = entry.getValue();
            for (int k = 0; k < item.merged.size(); ++k) {
                PdfDictionary merged = item.merged.get(k);
                PdfNumber ff = (PdfNumber)PdfReader.getPdfObject(merged.get(PdfName.F));
                int flags = 0;
                if (ff != null)
                    flags = ff.intValue();
                int page = item.page.get(k).intValue();
                PdfDictionary appDic = (PdfDictionary)PdfReader.getPdfObject(merged.get(PdfName.AP));
                if (appDic != null && (flags & PdfFormField.FLAGS_PRINT) != 0 && (flags & PdfFormField.FLAGS_HIDDEN) == 0) {
                    PdfObject obj = appDic.get(PdfName.N);
                    PdfAppearance app = null;
                    if (obj != null) {
                        PdfObject objReal = PdfReader.getPdfObject(obj);
                        if (obj instanceof PdfIndirectReference && !obj.isIndirect())
                            app = new PdfAppearance((PdfIndirectReference)obj);
                        else if (objReal instanceof PdfStream) {
                            ((PdfDictionary)objReal).put(PdfName.SUBTYPE, PdfName.FORM);
                            app = new PdfAppearance((PdfIndirectReference)obj);
                        }
                        else {
                            if (objReal != null && objReal.isDictionary()) {
                                PdfName as = (PdfName)PdfReader.getPdfObject(merged.get(PdfName.AS));
                                if (as != null) {
                                    PdfIndirectReference iref = (PdfIndirectReference)((PdfDictionary)objReal).get(as);
                                    if (iref != null) {
                                        app = new PdfAppearance(iref);
                                        if (iref.isIndirect()) {
                                            objReal = PdfReader.getPdfObject(iref);
                                            ((PdfDictionary)objReal).put(PdfName.SUBTYPE, PdfName.FORM);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if (app != null) {
                        Rectangle box = PdfReader.getNormalizedRectangle((PdfArray)PdfReader.getPdfObject(merged.get(PdfName.RECT)));
                        PdfContentByte cb = getOverContent(page);
                        cb.setLiteral("Q ");
                        cb.addTemplate(app, box.getLeft(), box.getBottom());
                        cb.setLiteral("q ");
                    }
                }
                if (partialFlattening.isEmpty())
                    continue;
                PdfDictionary pageDic = reader.getPageN(page);
                PdfArray annots = (PdfArray)PdfReader.getPdfObject(pageDic.get(PdfName.ANNOTS));
                if (annots == null)
                    continue;
                ArrayList<PdfObject> ar = annots.getArrayList();
                for (int idx = 0; idx < ar.size(); ++idx) {
                    PdfObject ran = ar.get(idx);
                    if (!ran.isIndirect())
                        continue;
                    PdfObject ran2 = item.widget_refs.get(k);
                    if (!ran2.isIndirect())
                        continue;
                    if (((PRIndirectReference)ran).getNumber() == ((PRIndirectReference)ran2).getNumber()) {
                        ar.remove(idx--);
                        PRIndirectReference wdref = (PRIndirectReference)ran2;
                        while (true) {
                            PdfDictionary wd = (PdfDictionary)PdfReader.getPdfObject(wdref);
                            PRIndirectReference parentRef = (PRIndirectReference)wd.get(PdfName.PARENT);
                            PdfReader.killIndirect(wdref);
                            if (parentRef == null) { 
                                for (int fr = 0; fr < acroFds.size(); ++fr) {
                                    PdfObject h = acroFds.get(fr);
                                    if (h.isIndirect() && ((PRIndirectReference)h).getNumber() == wdref.getNumber()) {
                                        acroFds.remove(fr);
                                        --fr;
                                    }
                                }
                                break;
                            }
                            PdfDictionary parent = (PdfDictionary)PdfReader.getPdfObject(parentRef);
                            PdfArray kids = (PdfArray)PdfReader.getPdfObject(parent.get(PdfName.KIDS));
                            ArrayList<PdfObject> kar = kids.getArrayList();
                            for (int fr = 0; fr < kar.size(); ++fr) {
                                PdfObject h = kar.get(fr);
                                if (h.isIndirect() && ((PRIndirectReference)h).getNumber() == wdref.getNumber()) {
                                    kar.remove(fr);
                                    --fr;
                                }
                            }
                            if (!kar.isEmpty())
                                break;
                            wdref = parentRef;
                        }
                    }
                }
                if (ar.isEmpty()) {
                    PdfReader.killIndirect(pageDic.get(PdfName.ANNOTS));
                    pageDic.remove(PdfName.ANNOTS);
                }
            }
        }
        if (!fieldsAdded && partialFlattening.isEmpty()) {
            for (int page = 1; page <= reader.getNumberOfPages(); ++page) {
                PdfDictionary pageDic = reader.getPageN(page);
                PdfArray annots = (PdfArray)PdfReader.getPdfObject(pageDic.get(PdfName.ANNOTS));
                if (annots == null)
                    continue;
                ArrayList<PdfObject> ar = annots.getArrayList();
                for (int idx = 0; idx < ar.size(); ++idx) {
                    PdfObject annoto = PdfReader.getPdfObject(ar.get(idx));
                    if ((annoto instanceof PdfIndirectReference) && !annoto.isIndirect())
                        continue;
                    if (!annoto.isDictionary() || PdfName.WIDGET.equals(((PdfDictionary)annoto).get(PdfName.SUBTYPE))) {
                        ar.remove(idx);
                        --idx;
                    }
                }
                if (ar.isEmpty()) {
                    PdfReader.killIndirect(pageDic.get(PdfName.ANNOTS));
                    pageDic.remove(PdfName.ANNOTS);
                }
            }
            eliminateAcroformObjects();
        }
    }

    void eliminateAcroformObjects() {
        PdfObject acro = reader.getCatalog().get(PdfName.ACROFORM);
        if (acro == null)
            return;
        PdfDictionary acrodic = (PdfDictionary)PdfReader.getPdfObject(acro);
        reader.killXref(acrodic.get(PdfName.XFA));
        acrodic.remove(PdfName.XFA);
        PdfObject iFields = acrodic.get(PdfName.FIELDS);
        if (iFields != null) {
            PdfDictionary kids = new PdfDictionary();
            kids.put(PdfName.KIDS, iFields);
            sweepKids(kids);
            PdfReader.killIndirect(iFields);
            acrodic.put(PdfName.FIELDS, new PdfArray());
        }


    }
    
    void sweepKids(PdfObject obj) {
        PdfObject oo = PdfReader.killIndirect(obj);
        if (oo == null || !oo.isDictionary())
            return;
        PdfDictionary dic = (PdfDictionary)oo;
        PdfArray kids = (PdfArray)PdfReader.killIndirect(dic.get(PdfName.KIDS));
        if (kids == null)
            return;
        ArrayList<PdfObject> ar = kids.getArrayList();
        for (int k = 0; k < ar.size(); ++k) {
            sweepKids(ar.get(k));
        }
    }
    
    private void flatFreeTextFields() 
    {
        if (append)
            throw new IllegalArgumentException("FreeText flattening is not supported in append mode.");
        
        for (int page = 1; page <= reader.getNumberOfPages(); ++page) 
        {
            PdfDictionary pageDic = reader.getPageN(page);
            PdfArray annots = (PdfArray)PdfReader.getPdfObject(pageDic.get(PdfName.ANNOTS));
            if (annots == null)
                continue;
            ArrayList<PdfObject> ar = annots.getArrayList();
            for (int idx = 0; idx < ar.size(); ++idx) 
            {
                PdfObject annoto = PdfReader.getPdfObject(ar.get(idx));
                if ((annoto instanceof PdfIndirectReference) && !annoto.isIndirect())
                    continue;
                
                PdfDictionary annDic = (PdfDictionary)annoto;
                 if (!((PdfName)annDic.get(PdfName.SUBTYPE)).equals(PdfName.FREETEXT)) 
                    continue;
                PdfNumber ff = (PdfNumber)PdfReader.getPdfObject(annDic.get(PdfName.F));
                int flags = (ff != null) ? ff.intValue() : 0;
            
                if ( (flags & PdfFormField.FLAGS_PRINT) != 0 && (flags & PdfFormField.FLAGS_HIDDEN) == 0) 
                {
                    PdfObject obj1 = annDic.get(PdfName.AP);
                    if (obj1 == null) 
                        continue;
                    PdfDictionary appDic = (obj1 instanceof PdfIndirectReference) ?
                            (PdfDictionary) PdfReader.getPdfObject(obj1) : (PdfDictionary) obj1;            
                    PdfObject obj = appDic.get(PdfName.N);
                    PdfAppearance app = null;
                    PdfObject objReal = PdfReader.getPdfObject(obj);
                    
                    if (obj instanceof PdfIndirectReference && !obj.isIndirect())
                        app = new PdfAppearance((PdfIndirectReference)obj);
                    else if (objReal instanceof PdfStream) 
                    {
                        ((PdfDictionary)objReal).put(PdfName.SUBTYPE, PdfName.FORM);
                        app = new PdfAppearance((PdfIndirectReference)obj);
                    }
                    else 
                    {
                        if (objReal.isDictionary()) 
                        {
                            PdfName as_p = (PdfName)PdfReader.getPdfObject(appDic.get(PdfName.AS));
                            if (as_p != null) 
                            {
                                PdfIndirectReference iref = (PdfIndirectReference)((PdfDictionary)objReal).get(as_p);
                                if (iref != null) 
                                {
                                    app = new PdfAppearance(iref);
                                    if (iref.isIndirect()) 
                                    {
                                        objReal = PdfReader.getPdfObject(iref);
                                        ((PdfDictionary)objReal).put(PdfName.SUBTYPE, PdfName.FORM);
                                    }
                                }
                            }
                        }
                    }
                    if (app != null) 
                    {
                        Rectangle box = PdfReader.getNormalizedRectangle((PdfArray)PdfReader.getPdfObject(annDic.get(PdfName.RECT)));
                        PdfContentByte cb = getOverContent(page);
                        cb.setLiteral("Q ");
                        cb.addTemplate(app, box.getLeft(), box.getBottom());
                        cb.setLiteral("q ");
                    }
                }
            }
            for (int idx = 0; idx < ar.size(); ++idx) 
            {
                PdfObject annoto = PdfReader.getPdfObject(ar.get(idx));
                if (annoto != null && annoto.isDictionary())
                {
                    PdfDictionary annot = (PdfDictionary)annoto;
                    if (PdfName.FREETEXT.equals(annot.get(PdfName.SUBTYPE)))
                    {
                        ar.remove(idx);
                        --idx;
                    }
                }
            }
            if (ar.isEmpty()) 
            {
                PdfReader.killIndirect(pageDic.get(PdfName.ANNOTS));
                pageDic.remove(PdfName.ANNOTS);
            }
        }
    }
    
    
    public PdfIndirectReference getPageReference(int page) {
        PdfIndirectReference ref = reader.getPageOrigRef(page);
        if (ref == null)
            throw new IllegalArgumentException("Invalid page number " + page);
        return ref;
    }
    
    
    public void addAnnotation(PdfAnnotation annot) {
        throw new RuntimeException("Unsupported in this context. Use PdfStamper.addAnnotation()");
    }
    
    void addDocumentField(PdfIndirectReference ref) {
        PdfDictionary catalog = reader.getCatalog();
        PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.ACROFORM), catalog);
        if (acroForm == null) {
            acroForm = new PdfDictionary();
            catalog.put(PdfName.ACROFORM, acroForm);
            markUsed(catalog);
        }
        PdfArray fields = (PdfArray)PdfReader.getPdfObject(acroForm.get(PdfName.FIELDS), acroForm);
        if (fields == null) {
            fields = new PdfArray();
            acroForm.put(PdfName.FIELDS, fields);
            markUsed(acroForm);
        }
        if (!acroForm.contains(PdfName.DA)) {
            acroForm.put(PdfName.DA, new PdfString("/Helv 0 Tf 0 g "));
            markUsed(acroForm);
        }
        fields.add(ref);
        markUsed(fields);
    }
    
    void addFieldResources() throws IOException {
        if (fieldTemplates.isEmpty())
            return;
        PdfDictionary catalog = reader.getCatalog();
        PdfDictionary acroForm = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.ACROFORM), catalog);
        if (acroForm == null) {
            acroForm = new PdfDictionary();
            catalog.put(PdfName.ACROFORM, acroForm);
            markUsed(catalog);
        }
        PdfDictionary dr = (PdfDictionary)PdfReader.getPdfObject(acroForm.get(PdfName.DR), acroForm);
        if (dr == null) {
            dr = new PdfDictionary();
            acroForm.put(PdfName.DR, dr);
            markUsed(acroForm);
        }
        markUsed(dr);
        for (PdfTemplate template: fieldTemplates) {
            PdfFormField.mergeResources(dr, (PdfDictionary)template.getResources(), this);
        }
        if (dr.get(PdfName.ENCODING) == null)
            dr.put(PdfName.ENCODING, PdfName.WIN_ANSI_ENCODING);
        PdfDictionary fonts = (PdfDictionary)PdfReader.getPdfObject(dr.get(PdfName.FONT));
        if (fonts == null) {
            fonts = new PdfDictionary();
            dr.put(PdfName.FONT, fonts);
        }
        if (!fonts.contains(PdfName.HELV)) {
            PdfDictionary dic = new PdfDictionary(PdfName.FONT);
            dic.put(PdfName.BASEFONT, PdfName.HELVETICA);
            dic.put(PdfName.ENCODING, PdfName.WIN_ANSI_ENCODING);
            dic.put(PdfName.NAME, PdfName.HELV);
            dic.put(PdfName.SUBTYPE, PdfName.TYPE1);
            fonts.put(PdfName.HELV, addToBody(dic).getIndirectReference());
        }
        if (!fonts.contains(PdfName.ZADB)) {
            PdfDictionary dic = new PdfDictionary(PdfName.FONT);
            dic.put(PdfName.BASEFONT, PdfName.ZAPFDINGBATS);
            dic.put(PdfName.NAME, PdfName.ZADB);
            dic.put(PdfName.SUBTYPE, PdfName.TYPE1);
            fonts.put(PdfName.ZADB, addToBody(dic).getIndirectReference());
        }
        if (acroForm.get(PdfName.DA) == null) {
            acroForm.put(PdfName.DA, new PdfString("/Helv 0 Tf 0 g "));
            markUsed(acroForm);
        }
    }
    
    void expandFields(PdfFormField field, ArrayList<PdfAnnotation> allAnnots) {
        allAnnots.add(field);
        ArrayList<PdfFormField> kids = field.getKids();
        if (kids != null) {
            for (int k = 0; k < kids.size(); ++k)
                expandFields(kids.get(k), allAnnots);
        }
    }

    void addAnnotation(PdfAnnotation annot, PdfDictionary pageN) {
        try {
            ArrayList<PdfAnnotation> allAnnots = new ArrayList<PdfAnnotation>();
            if (annot.isForm()) {
                fieldsAdded = true;
                getAcroFields();
                PdfFormField field = (PdfFormField)annot;
                if (field.getParent() != null)
                    return;
                expandFields(field, allAnnots);
            }
            else
                allAnnots.add(annot);
            for (int k = 0; k < allAnnots.size(); ++k) {
                annot = allAnnots.get(k);
                if (annot.getPlaceInPage() > 0)
                    pageN = reader.getPageN(annot.getPlaceInPage());
                if (annot.isForm()) {
                    if (!annot.isUsed()) {
                        HashSet<PdfTemplate> templates = annot.getTemplates();
                        if (templates != null)
                            fieldTemplates.addAll(templates);
                    }
                    PdfFormField field = (PdfFormField)annot;
                    if (field.getParent() == null)
                        addDocumentField(field.getIndirectReference());
                }
                if (annot.isAnnotation()) {
                    PdfObject pdfobj = PdfReader.getPdfObject(pageN.get(PdfName.ANNOTS), pageN);
                    PdfArray annots = null;
                    if (pdfobj == null || !pdfobj.isArray()) {
                        annots = new PdfArray();
                        pageN.put(PdfName.ANNOTS, annots);
                        markUsed(pageN);
                    }
                    else 
                       annots = (PdfArray)pdfobj;
                    annots.add(annot.getIndirectReference());
                    markUsed(annots);
                    if (!annot.isUsed()) {
                        PdfRectangle rect = (PdfRectangle)annot.get(PdfName.RECT);
                        if (rect != null && (rect.left() != 0 || rect.right() != 0 || rect.top() != 0 || rect.bottom() != 0)) {
                            int rotation = reader.getPageRotation(pageN);
                            Rectangle pageSize = reader.getPageSizeWithRotation(pageN);
                            switch (rotation) {
                                case 90:
                                    annot.put(PdfName.RECT, new PdfRectangle(
                                    pageSize.getTop() - rect.bottom(),
                                    rect.left(),
                                    pageSize.getTop() - rect.top(),
                                    rect.right()));
                                    break;
                                case 180:
                                    annot.put(PdfName.RECT, new PdfRectangle(
                                    pageSize.getRight() - rect.left(),
                                    pageSize.getTop() - rect.bottom(),
                                    pageSize.getRight() - rect.right(),
                                    pageSize.getTop() - rect.top()));
                                    break;
                                case 270:
                                    annot.put(PdfName.RECT, new PdfRectangle(
                                    rect.bottom(),
                                    pageSize.getRight() - rect.left(),
                                    rect.top(),
                                    pageSize.getRight() - rect.right()));
                                    break;
                            }
                        }
                    }
                }
                if (!annot.isUsed()) {
                    annot.setUsed();
                    addToBody(annot, annot.getIndirectReference());
                }
            }
        }
        catch (IOException e) {
            throw new ExceptionConverter(e);
        }
    }
    
    void addAnnotation(PdfAnnotation annot, int page) {
        addAnnotation(annot, reader.getPageN(page));
    }

    private void outlineTravel(PRIndirectReference outline) {
        while (outline != null) {
            PdfDictionary outlineR = (PdfDictionary)PdfReader.getPdfObjectRelease(outline);
            PRIndirectReference first = (PRIndirectReference)outlineR.get(PdfName.FIRST);
            if (first != null) {
                outlineTravel(first);
            }
            PdfReader.killIndirect(outlineR.get(PdfName.DEST));
            PdfReader.killIndirect(outlineR.get(PdfName.A));
            PdfReader.killIndirect(outline);
            outline = (PRIndirectReference)outlineR.get(PdfName.NEXT);
        }
    }

    void deleteOutlines() {
        PdfDictionary catalog = reader.getCatalog();
        PRIndirectReference outlines = (PRIndirectReference)catalog.get(PdfName.OUTLINES);
        if (outlines == null)
            return;
        outlineTravel(outlines);
        PdfReader.killIndirect(outlines);
        catalog.remove(PdfName.OUTLINES);
        markUsed(catalog);
    }
    
    void setJavaScript() throws IOException {
        HashMap<String, PdfObject> djs = pdf.getDocumentLevelJS();
        if (djs.isEmpty())
            return;
        PdfDictionary catalog = reader.getCatalog();
        PdfDictionary names = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.NAMES), catalog);
        if (names == null) {
            names = new PdfDictionary();
            catalog.put(PdfName.NAMES, names);
            markUsed(catalog);
        }
        markUsed(names);
        PdfDictionary tree = PdfNameTree.writeTree(djs, this);
        names.put(PdfName.JAVASCRIPT, addToBody(tree).getIndirectReference());
    }

    void addFileAttachments() throws IOException {
        HashMap<String, PdfObject> fs = pdf.getDocumentFileAttachment();
        if (fs.isEmpty())
            return;
        PdfDictionary catalog = reader.getCatalog();
        PdfDictionary names = (PdfDictionary)PdfReader.getPdfObject(catalog.get(PdfName.NAMES), catalog);
        if (names == null) {
            names = new PdfDictionary();
            catalog.put(PdfName.NAMES, names);
            markUsed(catalog);
        }
        markUsed(names);
        HashMap<String, PdfObject> old = PdfNameTree.readTree((PdfDictionary)PdfReader.getPdfObjectRelease(names.get(PdfName.EMBEDDEDFILES)));
        for (Map.Entry<String, PdfObject> entry: fs.entrySet()) {
            String name = entry.getKey();
            int k = 0;
            String nn = name;
            while (old.containsKey(nn)) {
                ++k;
                nn += " " + k;
            }
            old.put(nn, entry.getValue());
        }
        PdfDictionary tree = PdfNameTree.writeTree(old, this);
        names.put(PdfName.EMBEDDEDFILES, addToBody(tree).getIndirectReference());
    }

    
    void makePackage( PdfCollection collection ) {
        PdfDictionary catalog = reader.getCatalog();
           catalog.put( PdfName.COLLECTION, collection );
    }
 
    void setOutlines() throws IOException {
        if (newBookmarks == null)
            return;
        deleteOutlines();
        if (newBookmarks.isEmpty())
            return;
        PdfDictionary catalog = reader.getCatalog();
        boolean namedAsNames = (catalog.get(PdfName.DESTS) != null);
        writeOutlines(catalog, namedAsNames);
        markUsed(catalog);
    }
        
    
    public void setViewerPreferences(int preferences) {
        useVp = true;
        this.viewerPreferences.setViewerPreferences(preferences);
    }
    
    
    public void addViewerPreference(PdfName key, PdfObject value) {
        useVp = true;
        this.viewerPreferences.addViewerPreference(key, value);
    }
    
    
    public void setSigFlags(int f) {
        sigFlags |= f;
    }
    
        
    public void setPageAction(PdfName actionType, PdfAction action) throws PdfException {
        throw new UnsupportedOperationException("Use setPageAction(PdfName actionType, PdfAction action, int page)");
    }

        
    void setPageAction(PdfName actionType, PdfAction action, int page) throws PdfException {
        if (!actionType.equals(PAGE_OPEN) && !actionType.equals(PAGE_CLOSE))
            throw new PdfException("Invalid page additional action type: " + actionType.toString());
        PdfDictionary pg = reader.getPageN(page);
        PdfDictionary aa = (PdfDictionary)PdfReader.getPdfObject(pg.get(PdfName.AA), pg);
        if (aa == null) {
            aa = new PdfDictionary();
            pg.put(PdfName.AA, aa);
            markUsed(pg);
        }
        aa.put(actionType, action);
        markUsed(aa);
    }

    
    public void setDuration(int seconds) {
        throw new UnsupportedOperationException("Use setPageAction(PdfName actionType, PdfAction action, int page)");
    }
    
    
    public void setTransition(PdfTransition transition) {
        throw new UnsupportedOperationException("Use setPageAction(PdfName actionType, PdfAction action, int page)");
    }

    
    void setDuration(int seconds, int page) {
        PdfDictionary pg = reader.getPageN(page);
        if (seconds < 0)
            pg.remove(PdfName.DUR);
        else
            pg.put(PdfName.DUR, new PdfNumber(seconds));
        markUsed(pg);
    }
    
    
    void setTransition(PdfTransition transition, int page) {
        PdfDictionary pg = reader.getPageN(page);
        if (transition == null)
            pg.remove(PdfName.TRANS);
        else
            pg.put(PdfName.TRANS, transition.getTransitionDictionary());
        markUsed(pg);
    }

    protected void markUsed(PdfObject obj) {
        if (append && obj != null) {
            PRIndirectReference ref = null;
            if (obj.type() == PdfObject.INDIRECT)
                ref = (PRIndirectReference)obj;
            else
                ref = obj.getIndRef();
            if (ref != null)
                marked.put(ref.getNumber(), 1);
        }
    }
    
    protected void markUsed(int num) {
        if (append)
            marked.put(num, 1);
    }
    
    
    boolean isAppend() {
        return append;
    }
        
    
    public void setAdditionalAction(PdfName actionType, PdfAction action) throws PdfException {
        if (!(actionType.equals(DOCUMENT_CLOSE) ||
        actionType.equals(WILL_SAVE) ||
        actionType.equals(DID_SAVE) ||
        actionType.equals(WILL_PRINT) ||
        actionType.equals(DID_PRINT))) {
            throw new PdfException("Invalid additional action type: " + actionType.toString());
        }
        PdfDictionary aa = (PdfDictionary)PdfReader.getPdfObject(reader.getCatalog().get(PdfName.AA));
        if (aa == null) {
            if (action == null)
                return;
            aa = new PdfDictionary();
            reader.getCatalog().put(PdfName.AA, aa);
        }
        markUsed(aa);
        if (action == null)
            aa.remove(actionType);
        else
            aa.put(actionType, action);
    }

    
    public void setOpenAction(PdfAction action) {
        openAction = action;
    }
    
    
    public void setOpenAction(String name) {
        throw new UnsupportedOperationException("Open actions by name are not supported.");
    }
    
    
    public void setThumbnail(com.lowagie.text.Image image) {
        throw new UnsupportedOperationException("Use PdfStamper.setThumbnail().");
    }
    
    void setThumbnail(Image image, int page) throws PdfException, DocumentException {
        PdfIndirectReference thumb = getImageReference(addDirectImageSimple(image));
        reader.resetReleasePage();
        PdfDictionary dic = reader.getPageN(page);
        dic.put(PdfName.THUMB, thumb);
        reader.resetReleasePage();
    }

    public PdfContentByte getDirectContentUnder() {
        throw new UnsupportedOperationException("Use PdfStamper.getUnderContent() or PdfStamper.getOverContent()");
    }

    public PdfContentByte getDirectContent() {
        throw new UnsupportedOperationException("Use PdfStamper.getUnderContent() or PdfStamper.getOverContent()");
    }
    
    
    protected void readOCProperties() {
        if (!documentOCG.isEmpty()) {
            return;
        }
        PdfDictionary dict = reader.getCatalog().getAsDict(PdfName.OCPROPERTIES);
        if (dict == null) {
            return;
        }
        PdfArray ocgs = dict.getAsArray(PdfName.OCGS);
        PdfIndirectReference ref;
        PdfLayer layer;
        HashMap<String, PdfLayer> ocgmap = new HashMap<String, PdfLayer>();
        for (Iterator<PdfObject> i = ocgs.listIterator(); i.hasNext(); ) {
            ref = (PdfIndirectReference)i.next();
            layer = new PdfLayer(null);
            layer.setRef(ref);
            layer.setOnPanel(false);
            layer.merge((PdfDictionary)PdfReader.getPdfObject(ref));
            ocgmap.put(ref.toString(), layer);
        }
        PdfDictionary d = dict.getAsDict(PdfName.D);
        PdfArray off = d.getAsArray(PdfName.OFF);
        if (off != null) {
            for (Iterator<PdfObject> i = off.listIterator(); i.hasNext(); ) {
                ref = (PdfIndirectReference)i.next();
                layer = ocgmap.get(ref.toString());
                layer.setOn(false);
            }
        }
        PdfArray order = d.getAsArray(PdfName.ORDER);
        if (order != null) {
            addOrder(null, order, ocgmap);
        }
        documentOCG.addAll(ocgmap.values());
        OCGRadioGroup = d.getAsArray(PdfName.RBGROUPS);
        OCGLocked = d.getAsArray(PdfName.LOCKED);
    }
    
    
    private void addOrder(PdfLayer parent, PdfArray arr, Map<String, PdfLayer> ocgmap) {
        PdfObject obj;
        PdfLayer layer;
        for (int i = 0; i < arr.size(); i++) {
            obj = arr.getPdfObject(i);
            if (obj.isIndirect()) {
                layer = ocgmap.get(obj.toString());
                layer.setOnPanel(true);
                registerLayer(layer);
                if (parent != null) {
                    parent.addChild(layer);
                }
                if (arr.size() > i + 1 && arr.getPdfObject(i + 1).isArray()) {
                    i++;
                    addOrder(layer, (PdfArray)arr.getPdfObject(i), ocgmap);
                }
            }
            else if (obj.isArray()) {
                ArrayList<PdfObject> sub = ((PdfArray)obj).getArrayList();
                if (sub.isEmpty()) return;
                obj = sub.get(0);
                if (obj.isString()) {
                    layer = new PdfLayer(sub.get(0).toString());
                    layer.setOnPanel(true);
                    registerLayer(layer);
                    if (parent != null) {
                        parent.addChild(layer);
                    }
                    PdfArray array = new PdfArray();
                    for (PdfObject o: sub) {
                        array.add(o);
                    }
                    addOrder(layer, array, ocgmap);
                }
                else {
                    addOrder(parent, (PdfArray)obj, ocgmap);
                }
            }
        }
    }
    
    
    public Map<String, PdfLayer> getPdfLayers() {
        if (documentOCG.isEmpty()) {
            readOCProperties();
        }
        HashMap<String, PdfLayer> map = new HashMap<String, PdfLayer>();
        PdfLayer layer;
        String key;
        for (Iterator<PdfOCG> i = documentOCG.iterator(); i.hasNext(); ) {
            layer = (PdfLayer)i.next();
            if (layer.getTitle() == null) {
                key = layer.getAsString(PdfName.NAME).toString();
            }
            else {
                key = layer.getTitle();
            }
            if (map.containsKey(key)) {
                int seq = 2;
                String tmp = key + "(" + seq + ")";
                while (map.containsKey(tmp)) {
                    seq++;
                    tmp = key + "(" + seq + ")";
                }
                key = tmp;
            }
            map.put(key, layer);
        }
        return map;
    }
    
    static class PageStamp {
        
        PdfDictionary pageN;
        StampContent under;
        StampContent over;
        PageResources pageResources;
        int replacePoint = 0;
        
        PageStamp(PdfStamperImp stamper, PdfReader reader, PdfDictionary pageN) {
            this.pageN = pageN;
            pageResources = new PageResources();
            PdfDictionary resources = (PdfDictionary)PdfReader.getPdfObject(pageN.get(PdfName.RESOURCES));
            pageResources.setOriginalResources(resources, stamper.namePtr);
        }
    }
}
