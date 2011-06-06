

package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.ArrayList;

import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;



public class PdfPages {
    
    private ArrayList<PdfIndirectReference> pages = new ArrayList<PdfIndirectReference>();
    private ArrayList<PdfIndirectReference> parents = new ArrayList<PdfIndirectReference>();
    private int leafSize = 10;
    private PdfWriter writer;
    private PdfIndirectReference topParent;
    
    
    

    
    PdfPages(PdfWriter writer) {
        this.writer = writer;
    }
    
    void addPage(PdfDictionary page) {
        try {
            if ((pages.size() % leafSize) == 0)
                parents.add(writer.getPdfIndirectReference());
            PdfIndirectReference parent = parents.get(parents.size() - 1);
            page.put(PdfName.PARENT, parent);
            PdfIndirectReference current = writer.getCurrentPage();
            writer.addToBody(page, current);
            pages.add(current);
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    PdfIndirectReference addPageRef(PdfIndirectReference pageRef) {
        try {
            if ((pages.size() % leafSize) == 0)
                parents.add(writer.getPdfIndirectReference());
            pages.add(pageRef);
            return parents.get(parents.size() - 1);
        }
        catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }
    
    
    PdfIndirectReference writePageTree() throws IOException {
        if (pages.isEmpty())
            throw new IOException("The document has no pages.");
        int leaf = 1;
        ArrayList<PdfIndirectReference> tParents = parents;
        ArrayList<PdfIndirectReference> tPages = pages;
        ArrayList<PdfIndirectReference> nextParents = new ArrayList<PdfIndirectReference>();
        while (true) {
            leaf *= leafSize;
            int stdCount = leafSize;
            int rightCount = tPages.size() % leafSize;
            if (rightCount == 0)
                rightCount = leafSize;
            for (int p = 0; p < tParents.size(); ++p) {
                int count;
                int thisLeaf = leaf;
                if (p == tParents.size() - 1) {
                    count = rightCount;
                    thisLeaf = pages.size() % leaf;
                    if (thisLeaf == 0)
                        thisLeaf = leaf;
                }
                else
                    count = stdCount;
                PdfDictionary top = new PdfDictionary(PdfName.PAGES);
                top.put(PdfName.COUNT, new PdfNumber(thisLeaf));
                PdfArray kids = new PdfArray();
                ArrayList<PdfObject> internal = kids.getArrayList();
                internal.addAll(tPages.subList(p * stdCount, p * stdCount + count));
                top.put(PdfName.KIDS, kids);
                if (tParents.size() > 1) {
                    if ((p % leafSize) == 0)
                        nextParents.add(writer.getPdfIndirectReference());
                    top.put(PdfName.PARENT, nextParents.get(p / leafSize));
                }
                writer.addToBody(top, tParents.get(p));
            }
            if (tParents.size() == 1) {
                topParent = tParents.get(0);
                return topParent;
            }
            tPages = tParents;
            tParents = nextParents;
            nextParents = new ArrayList<PdfIndirectReference>();
        }
    }
    
    PdfIndirectReference getTopParent() {
        return topParent;
    }
    
    void setLinearMode(PdfIndirectReference topParent) {
        if (parents.size() > 1)
            throw new RuntimeException("Linear page mode can only be called with a single parent.");
        if (topParent != null) {
            this.topParent = topParent;
            parents.clear();
            parents.add(topParent);
        }
        leafSize = 10000000;
    }

    void addPage(PdfIndirectReference page) {
        pages.add(page);
    }

    int reorderPages(int order[]) throws DocumentException {
        if (order == null)
            return pages.size();
        if (parents.size() > 1)
            throw new DocumentException("Page reordering requires a single parent in the page tree. Call PdfWriter.setLinearMode() after open.");
        if (order.length != pages.size())
            throw new DocumentException("Page reordering requires an array with the same size as the number of pages.");
        int max = pages.size();
        boolean temp[] = new boolean[max];
        for (int k = 0; k < max; ++k) {
            int p = order[k];
            if (p < 1 || p > max)
                throw new DocumentException("Page reordering requires pages between 1 and " + max + ". Found " + p + ".");
            if (temp[p - 1])
                throw new DocumentException("Page reordering requires no page repetition. Page " + p + " is repeated.");
            temp[p - 1] = true;
        }
        PdfIndirectReference copy[] = pages.toArray(new PdfIndirectReference[pages.size()]);
        for (int k = 0; k < max; ++k) {
            pages.set(k, copy[order[k] - 1]);
        }
        return max;
    }
}