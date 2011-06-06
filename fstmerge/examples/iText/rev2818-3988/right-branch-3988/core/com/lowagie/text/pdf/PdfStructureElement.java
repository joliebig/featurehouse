

package com.lowagie.text.pdf;


public class PdfStructureElement extends PdfStructureBase {
    
    private PdfStructureElement parent;
    private PdfStructureTreeRoot top;
    
    private Integer mcid; 
    private boolean hasPageMark = false;

    
    public PdfStructureElement(PdfStructureElement parent, PdfName structureType) {
      top = parent.top;
      init( parent, structureType );
      this.parent = parent;
      put( PdfName.P, parent.getIndRef() );
    }

    
    public PdfStructureElement( PdfStructureElement parent, PdfName structType, PdfIndirectReference ref ) {
      top = parent.top;
      init( parent, structType, ref );
      this.parent = parent;
      put( PdfName.P, parent.getIndRef() );
    }

    
    public PdfStructureElement(PdfStructureTreeRoot parent, PdfName structureType) {
        top = parent;
        init(parent, structureType);
        put(PdfName.P, parent.getIndRef());
    }

    
    public static PdfStructureElement createNextElement( PdfStructureElement parent, PdfName structureType ) {
        PdfStructureElement elem = new PdfStructureElement( parent, structureType );
        elem.getMCID();
        return elem;
    }

    
    public PdfWriter getWriter() {
        return top.getWriter();
    }

    private void init(PdfStructureBase parent, PdfName structureType) {
        init( parent, structureType, getWriter().getPdfIndirectReference() );
    }

    private void init( PdfStructureBase parent, PdfName structureType, PdfIndirectReference ref ) {
      parent.addKid( this );
      if (structureType != null) {
        put( PdfName.S, structureType );
      }
      setIndRef( ref );
    }

    
    public PdfDictionary getParent() {
        return parent;
    }

    
    public int getMCID() {
        checkKids(); 
        if (mcid == null) {
            mcid = new Integer( top.getNextMCID() );
        }

        return mcid.intValue();
    }

    
    public void setMCID( int id ) {
        checkKids();

        if (mcid != null) {
            throw new RuntimeException( "this element already has an MCID" );
        }

        mcid = new Integer( id );
    }

    
    public void setMarkedContent( PdfIndirectReference pageRef ) {
        checkKids();
 
        
        
        
        if (contains( PdfName.K) ) {
          PdfArray structIDs = null;
          PdfObject curKObj = get( PdfName.K );
          if (curKObj == null || curKObj.isNumber()) {
              structIDs = new PdfArray();
              
              if (curKObj != null) {
                  structIDs.add( curKObj );
              }
              put( PdfName.K, structIDs );
          } else if (curKObj.isArray()) {
              structIDs = (PdfArray) curKObj;
          } else {
              throw new IllegalArgumentException( "Unknown object at /K " + curKObj.getClass().toString() );
          }
          structIDs.add( new PdfStructureMC( getMCID(), pageRef ) );
        } else {
          
          put( PdfName.K,  new PdfNumber( getMCID() ) );
          put(PdfName.PG, pageRef);
        }
    }

    
    public void setMarkedObject( PdfIndirectReference objRef, PdfIndirectReference pageRef ) {
        checkKids();

        if (objRef == null) throw new NullPointerException( "object reference must be valid" );

        
        PdfStructureObj structObj = new PdfStructureObj( objRef, pageRef );
        put( PdfName.K, structObj );

        setObjMark();
    }

    
    void setPageMark( int page ) {
        checkKids();
        if (!hasPageMark) {
            mcid = new Integer( top.setPageMark( page, getIndRef() ) );
            hasPageMark = true;
        }
    }

    
    private void setObjMark() {
      top.setObjMark( getMCID(), getIndRef() );
    }

    
    private void checkKids() {
        if (kids != null) {
          throw new IllegalArgumentException( "This structure element already has children" );
        }
    }

    
    class PdfStructureMC extends PdfDictionary {
        public PdfStructureMC( int id, PdfIndirectReference pageRef ) {
            super( PdfName.MCR );

            put( PdfName.MCID, new PdfNumber( id ) );
            if (pageRef != null) {
                put( PdfName.PG, pageRef );
            }
        }
    }

    
    class PdfStructureObj extends PdfDictionary {
        public PdfStructureObj( PdfIndirectReference objRef, PdfIndirectReference pageRef ) {
            super( PdfName.OBJR );
            put( PdfName.OBJ, objRef );
            if (pageRef != null) {
                put( PdfName.PG, pageRef );
            }
        }
    }
}
