

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
}
