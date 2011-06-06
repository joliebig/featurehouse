

package com.lowagie.text.rtf.document;

import com.lowagie.text.rtf.document.output.RtfDataCache;
import com.lowagie.text.rtf.style.RtfParagraphStyle;



public class RtfDocumentSettings {

    
    private RtfDocument document = null;
    
    private boolean outputTableRowDefinitionAfter = true;
    
    private boolean outputDebugLineBreaks = true;
    
    private boolean alwaysGenerateSoftLinebreaks = false;
    
    private boolean alwaysUseUnicode = true;
    
    private int dataCacheStyle = RtfDataCache.CACHE_MEMORY;
    
    private boolean writeImageScalingInformation = false;
    
    private boolean imagePDFConformance = true;
    
    private int protectionLevel = RtfProtection.LEVEL_NONE;
    
    private String protectionHash = null;
    
    private String writereservhash = null; 
    
    private boolean readOnlyRecommended = false;
    
    private boolean imageWrittenAsBinary = true;
    
    
    public RtfDocumentSettings(RtfDocument document) {
        this.document = document;
    }
    
    
    public boolean isOutputDebugLineBreaks() {
        return outputDebugLineBreaks;
    }
    
    
    public void setOutputDebugLineBreaks(boolean outputDebugLineBreaks) {
        this.outputDebugLineBreaks = outputDebugLineBreaks;
    }
    
    
    public boolean isOutputTableRowDefinitionAfter() {
        return outputTableRowDefinitionAfter;
    }
    
    
    public void setOutputTableRowDefinitionAfter(
            boolean outputTableRowDefinitionAfter) {
        this.outputTableRowDefinitionAfter = outputTableRowDefinitionAfter;
    }
    
    
    public boolean isAlwaysGenerateSoftLinebreaks() {
        return this.alwaysGenerateSoftLinebreaks;
    }

    
    public void setAlwaysGenerateSoftLinebreaks(boolean alwaysGenerateSoftLinebreaks) {
        this.alwaysGenerateSoftLinebreaks = alwaysGenerateSoftLinebreaks;
    }
    
    
    public boolean isAlwaysUseUnicode() {
        return this.alwaysUseUnicode;
    }
    
    
    public void setAlwaysUseUnicode(boolean alwaysUseUnicode) {
        this.alwaysUseUnicode = alwaysUseUnicode;
    }

    
    public void registerParagraphStyle(RtfParagraphStyle rtfParagraphStyle) {
        this.document.getDocumentHeader().registerParagraphStyle(rtfParagraphStyle);
    }
    
    
    public void setDataCacheStyle(int dataCacheStyle) {
        switch(dataCacheStyle) {
            case RtfDataCache.CACHE_MEMORY_EFFICIENT:    
                this.dataCacheStyle = RtfDataCache.CACHE_MEMORY_EFFICIENT;
                break;
            case RtfDataCache.CACHE_DISK:                 
                this.dataCacheStyle = RtfDataCache.CACHE_DISK;
                break;
            default:
            case RtfDataCache.CACHE_MEMORY:             
                this.dataCacheStyle = RtfDataCache.CACHE_MEMORY;
                break;
        }
    }
    
    
    public int getDataCacheStyle() {
        return this.dataCacheStyle;
    }

    
    
    public boolean isImagePDFConformance() {
        return this.imagePDFConformance;
    }

    
    
    public void setImagePDFConformance(boolean imagePDFConformance) {
        this.imagePDFConformance = imagePDFConformance;
    }

    
    
    public boolean isWriteImageScalingInformation() {
        return this.writeImageScalingInformation;
    }

    
    
    public void setWriteImageScalingInformation(boolean writeImageScalingInformation) {
        this.writeImageScalingInformation = writeImageScalingInformation;
    }
    
    
    public void setOptionsForMSWord2000And97() {
        this.setOutputTableRowDefinitionAfter(true);
        this.setWriteImageScalingInformation(true);
    }
    
    
    public void setOptionsForMSWordForMac() {
        this.setWriteImageScalingInformation(true);
    }
    
    
    public void setOptionsForMSWordXP() {
        this.setWriteImageScalingInformation(false);
    }

    
    public void setOptionsForOpenOfficeOrg() {
        this.setOutputTableRowDefinitionAfter(false);
    }
    
    
    public boolean setProtection(int level, String pwd) {
        boolean result = false;
        if(this.protectionHash == null) {
            if(!setProtectionLevel(level)) {
                result = false;
            }
            else
            {
                protectionHash = RtfProtection.generateHash(pwd);
                result = true;
            }
        }
        else {
            if(this.protectionHash.equals(RtfProtection.generateHash(pwd))) {
                if(!setProtectionLevel(level)) {
                    result = false;
                }
                else
                {
                    protectionHash = RtfProtection.generateHash(pwd);
                    result = true;
                }
            }
        }
        return result;
    }
    
    
    public boolean unprotectDocument(String pwd) {
        boolean result = false;
        if (this.protectionHash.equals(RtfProtection.generateHash(pwd))) {
            this.protectionLevel =  RtfProtection.LEVEL_NONE;
            this.protectionHash = null;
            result = true;
        }
        return result;
    }
    
    
    public boolean setProtectionLevel(int level) {
        boolean result = false;
        switch(level) {
        case RtfProtection.LEVEL_NONE:
            if(this.protectionHash == null) {
                break;
            }
        case RtfProtection.LEVEL_ANNOTPROT:
        case RtfProtection.LEVEL_FORMPROT:
        case RtfProtection.LEVEL_REVPROT:
        case RtfProtection.LEVEL_READPROT:
            this.protectionLevel = level;
            result = true;
            break;
           default:
        }
        return result;
    }
    
    
    public void setPasswordHash(String pwd) {
        if(pwd != null && pwd.length() != 8) return;
        this.protectionHash = pwd;
    }
    
    
    private int convertProtectionLevel() {
        int level = 0;
        switch(this.protectionLevel) {
        case RtfProtection.LEVEL_NONE:
            break;
        case RtfProtection.LEVEL_REVPROT:
            level = 0;
            break;
        case RtfProtection.LEVEL_ANNOTPROT:
            level = 1;
            break;
        case RtfProtection.LEVEL_FORMPROT:
            level = 2;
            break;
        case RtfProtection.LEVEL_READPROT:
            level = 3;
            break;
        }
        return level;
        
    }
    
    
    public int getProtectionLevelRaw() {
        return this.protectionLevel;
    }
    
    
    public int getProtectionLevel() {
        return convertProtectionLevel();
    }
    
    
    public byte[] getProtectionLevelBytes() {
        return Integer.toString(convertProtectionLevel()).getBytes();
    }
    
    
    public boolean setNewPassword(String oldPwd, String newPwd) {
        boolean result = false;
        if (this.protectionHash.equals(RtfProtection.generateHash(oldPwd))) {
            this.protectionHash = RtfProtection.generateHash(newPwd);
            result = true;
        }
        return result;
    }
    
    
    public void setReadOnlyRecommended(boolean value) {
        this.readOnlyRecommended = value;
    }
    
    
    public boolean getReadOnlyRecommended() {
        return this.readOnlyRecommended;
    }
    
    
    public boolean isDocumentProtected() {
        return !(this.protectionHash == null);
    }
    
    
    public byte[] getProtectionHashBytes() {
        return this.protectionHash.getBytes();
    }

    
    public void setImageWrittenAsBinary(boolean imageWrittenAsBinary) {
        this.imageWrittenAsBinary = imageWrittenAsBinary;
    }
    
    
    public boolean isImageWrittenAsBinary() {
        return this.imageWrittenAsBinary;
    }
}
