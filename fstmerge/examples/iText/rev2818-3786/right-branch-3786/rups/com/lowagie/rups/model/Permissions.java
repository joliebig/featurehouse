

package com.lowagie.rups.model;

import com.lowagie.text.pdf.PdfWriter;


public class Permissions {

    
    protected boolean encrypted = true;
    
    protected byte[] ownerPassword = null;
    
    protected byte[] userPassword = null;
    
    protected int permissions = 0;
    
    protected int cryptoMode = 0;
    
    
    public boolean isEncrypted() {
        return encrypted;
    }
    
    public void setEncrypted(boolean encrypted) {
        this.encrypted = encrypted;
    }
    
    public byte[] getOwnerPassword() {
        return ownerPassword;
    }
    
    public void setOwnerPassword(byte[] ownerPassword) {
        this.ownerPassword = ownerPassword;
    }
    
    public byte[] getUserPassword() {
        return userPassword;
    }
    
    public void setUserPassword(byte[] userPassword) {
        this.userPassword = userPassword;
    }
    
    public int getPermissions() {
        return permissions;
    }
    
    public void setPermissions(int permissions) {
        this.permissions = permissions;
    }
    
    public int getCryptoMode() {
        return cryptoMode;
    }
    
    public void setCryptoMode(int cryptoMode) {
        this.cryptoMode = cryptoMode;
    }
    
    
    public boolean isAllowPrinting() {
        return
            !encrypted
            || (PdfWriter.ALLOW_PRINTING & permissions) == PdfWriter.ALLOW_PRINTING;
    }
    
    public boolean isAllowModifyContents(boolean decrypted) {
        return
            !encrypted
            || (PdfWriter.ALLOW_MODIFY_CONTENTS & permissions) == PdfWriter.ALLOW_MODIFY_CONTENTS;
    }
    
    public boolean isAllowCopy(boolean decrypted) {
        return
            !encrypted
            || (PdfWriter.ALLOW_COPY & permissions) == PdfWriter.ALLOW_COPY;
    }
    
    public boolean isAllowModifyAnnotations() {
        return
            !encrypted
            || (PdfWriter.ALLOW_MODIFY_ANNOTATIONS & permissions) == PdfWriter.ALLOW_MODIFY_ANNOTATIONS;
    }
    
    public boolean isAllowFillIn() {
        return
            !encrypted
            || (PdfWriter.ALLOW_FILL_IN & permissions) == PdfWriter.ALLOW_FILL_IN;
    }
    
    public boolean isAllowScreenReaders() {
        return
            !encrypted
            || (PdfWriter.ALLOW_SCREENREADERS & permissions) == PdfWriter.ALLOW_SCREENREADERS;
    }
    
    public boolean isAllowAssembly() {
        return
            !encrypted
            || (PdfWriter.ALLOW_ASSEMBLY & permissions) == PdfWriter.ALLOW_ASSEMBLY;
    }
    
    public boolean isAllowDegradedPrinting() {
        return
            !encrypted
            || (PdfWriter.ALLOW_DEGRADED_PRINTING & permissions) == PdfWriter.ALLOW_DEGRADED_PRINTING;
    }
}