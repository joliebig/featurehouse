
package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;


public class PdfStructureTreeRoot extends PdfStructureBase {

    
    private HashMap<Integer, PdfObject> parentTree = new HashMap<Integer, PdfObject>();
    
    private Map<Integer, Integer> pageMCIDs = new HashMap<Integer, Integer>();
    private int nextMark = 0;
    private PdfWriter writer;

    
    PdfStructureTreeRoot(PdfWriter writer) {
        super(PdfName.STRUCTTREEROOT);
        if (writer == null) {
            throw new NullPointerException( "PdfWriter param must not be null" );
        }
        this.writer = writer;
        setIndRef( writer.getPdfIndirectReference() );
    }

    
    public void mapRole(PdfName used, PdfName standard) {
        PdfDictionary rm = (PdfDictionary)get(PdfName.ROLEMAP);
        if (rm == null) {
            rm = new PdfDictionary();
            put(PdfName.ROLEMAP, rm);
        }
        rm.put(used, standard);
    }

    
    public PdfWriter getWriter() {
        return writer;
    }

    
    public int getNextMCID() {
        return nextMark++;
    }


    
    public void setPagesMCID( int pageIdx, int pageMCID ) {
      Integer idxObj = new Integer( pageIdx );
      if (!pageMCIDs.containsKey( idxObj )) {
        pageMCIDs.put( idxObj, new Integer( pageMCID ) );
        parentTree.put( idxObj, new PdfArray() );
      }
    }

    
    public int getMCIDForPage( int pageIdx ) {
      return (pageMCIDs.get( new Integer( pageIdx ) )).intValue();
      
    }

    
    public Integer getMCIDForPage( Integer pageIdx ) {
        return pageMCIDs.get( pageIdx );
    }

    
    public int setPageMark(int pageIdx, PdfIndirectReference struc) {
        Integer i = pageMCIDs.get( new Integer( pageIdx ) );
        
        PdfArray ar = (PdfArray)parentTree.get( i );
        if (ar == null) {
            ar = new PdfArray();
            parentTree.put(new Integer(pageIdx), ar);
        }
        ar.add(struc);
        return ar.size() - 1;
    }

    
    public void setObjMark( int objID, PdfIndirectReference strucRef) {
      Integer i = new Integer( objID );
      parentTree.put( i, strucRef );
    }

    private void nodeProcess(PdfDictionary struc, PdfIndirectReference reference) throws IOException {
        PdfArray ar = struc.getAsArray(PdfName.K);
        if (ar != null && !ar.getPdfObject(0).isNumber()) {
            for (int k = 0; k < ar.size(); ++k) {
                PdfStructureElement e = (PdfStructureElement)ar.getPdfObject(k);
                ar.set(k, e.getIndRef());
                nodeProcess(e, e.getIndRef());
            }
        }
        if (reference != null)
            writer.addToBody(struc, reference);
    }

    void buildTree() throws IOException {
        HashMap<Integer, PdfIndirectReference> numTree = new HashMap<Integer, PdfIndirectReference>();
        for (Integer i: parentTree.keySet()) {
            PdfObject obj = parentTree.get( i );
            
            if (obj instanceof PdfIndirectReference) {
                numTree.put( i, (PdfIndirectReference) obj );
            } else {
                numTree.put(i, writer.addToBody(obj).getIndirectReference());
            }
        }
        PdfDictionary dicTree = PdfNumberTree.writeTree(numTree, writer);
        if (dicTree != null)
            put(PdfName.PARENTTREE, writer.addToBody(dicTree).getIndirectReference());

        nodeProcess(this, getIndRef() );

        put( PdfName.PARENTTREENEXTKEY, new PdfNumber( nextMark ) );
    }
}
