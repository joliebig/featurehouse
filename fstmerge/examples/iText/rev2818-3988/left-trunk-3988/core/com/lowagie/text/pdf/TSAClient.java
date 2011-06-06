

package com.lowagie.text.pdf;


public interface TSAClient {
    
    public int getTokenSizeEstimate();
    
    
    public byte[] getTimeStampToken(PdfPKCS7 caller, byte[] imprint) throws Exception;
    
}