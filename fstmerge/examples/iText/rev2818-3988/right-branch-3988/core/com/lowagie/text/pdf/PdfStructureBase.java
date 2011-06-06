


package com.lowagie.text.pdf;



abstract class PdfStructureBase extends PdfDictionary {
  protected PdfArray kids; 

  public PdfStructureBase( PdfName dicType ) {
    super( dicType );
  }

  
  public PdfStructureBase() {
  }

  abstract protected PdfWriter getWriter();


  
  public PdfIndirectReference getIndRef() {
    if (super.getIndRef() == null) {
        setIndRef( getWriter().getPdfIndirectReference() );
    }
    return super.getIndRef();
  }

    
  public void addKid( PdfStructureElement kid ) throws IllegalArgumentException {
    if (kids == null) {
      if (contains( PdfName.K )) {
        throw new IllegalArgumentException( "this structure object already has a 'K' value" );
      }
      kids = new PdfArray();
      put( PdfName.K, kids );
    }

    kids.add( kid );
  }
}

