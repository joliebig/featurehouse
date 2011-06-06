

package com.lowagie.text.rtf.graphic;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.codec.wmf.MetaDo;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.document.output.RtfByteArrayBuffer;
import com.lowagie.text.rtf.style.RtfParagraphStyle;
import com.lowagie.text.rtf.text.RtfParagraph;


public class RtfImage extends RtfElement {
    
    
    private static final byte[] PICTURE_GROUP = "\\*\\shppict".getBytes();
    
    private static final byte[] PICTURE = "\\pict".getBytes();
    
    private static final byte[] PICTURE_JPEG = "\\jpegblip".getBytes();
    
    private static final byte[] PICTURE_PNG = "\\pngblip".getBytes();
    
    private static final byte[] PICTURE_WMF = "\\wmetafile8".getBytes();
    
    private static final byte[] PICTURE_WIDTH = "\\picw".getBytes();
    
    private static final byte[] PICTURE_HEIGHT = "\\pich".getBytes();
    
    private static final byte[] PICTURE_SCALED_WIDTH = "\\picwgoal".getBytes();
    
    private static final byte[] PICTURE_SCALED_HEIGHT = "\\pichgoal".getBytes();
    
    private static final byte[] PICTURE_SCALE_X = "\\picscalex".getBytes();
    
    private static final byte[] PICTURE_SCALE_Y = "\\picscaley".getBytes();
    
    private static final byte[] PICTURE_BINARY_DATA = "\\bin".getBytes();
    
    private static final int PIXEL_TWIPS_FACTOR = 15;
    
    
    private final int imageType;
    
    private final byte[][] imageData;
    
    private int alignment = Element.ALIGN_LEFT;
    
    private float width = 0;
    
    private float height = 0;
    
    private float plainWidth = 0;
    
    private float plainHeight = 0;
    
    private boolean topLevelElement = false;
    
    
    public RtfImage(RtfDocument doc, Image image) throws DocumentException
    {
        super(doc);
        imageType = image.getOriginalType();
        if (!(imageType == Image.ORIGINAL_JPEG || imageType == Image.ORIGINAL_BMP
                || imageType == Image.ORIGINAL_PNG || imageType == Image.ORIGINAL_WMF || imageType == Image.ORIGINAL_GIF)) {
            throw new DocumentException("Only BMP, PNG, WMF, GIF and JPEG images are supported by the RTF Writer");
        }
        alignment = image.getAlignment();
        width = image.getWidth();
        height = image.getHeight();
        plainWidth = image.getPlainWidth();
        plainHeight = image.getPlainHeight();
        this.imageData = getImageData(image);
    }
    
    
    private byte[][] getImageData(Image image) throws DocumentException 
    {
        final int WMF_PLACEABLE_HEADER_SIZE = 22;
        final RtfByteArrayBuffer bab = new RtfByteArrayBuffer();
        
        try {
            if(imageType == Image.ORIGINAL_BMP) {
                bab.append(MetaDo.wrapBMP(image));
            } else {                
                final byte[] iod = image.getOriginalData();
                if(iod == null) {
                    
                    final InputStream imageIn = image.getUrl().openStream();
                    if(imageType == Image.ORIGINAL_WMF) { 
                        for(int k = 0; k < WMF_PLACEABLE_HEADER_SIZE; k++) {
                            if(imageIn.read() < 0) throw new EOFException("while removing wmf placeable header");
                        }
                    }
                    bab.write(imageIn);
                    imageIn.close();
                    
                } else {
                    
                    if(imageType == Image.ORIGINAL_WMF) {
                        
                        bab.write(iod, WMF_PLACEABLE_HEADER_SIZE, iod.length - WMF_PLACEABLE_HEADER_SIZE);
                    } else {
                        bab.append(iod);
                    }
                    
                }
            }
            
            return bab.toByteArrayArray();
            
        } catch(IOException ioe) {
            throw new DocumentException(ioe.getMessage());
        }
    }
    
    
    
    public final static byte[] byte2charLUT = new byte[512]; 
    static {
        char c = '0';
        for(int k = 0; k < 16; k++) {
            for(int x = 0; x < 16; x++) {
                byte2charLUT[((k*16)+x)*2] = byte2charLUT[(((x*16)+k)*2)+1] = (byte)c;
            }
            if(++c == ':') c = 'a';
        }
    }
    
    
    private void writeImageDataHexEncoded(final OutputStream bab) throws IOException
    {
        int cnt = 0;
        for(int k = 0; k < imageData.length; k++) {
            final byte[] chunk = imageData[k];
            for(int x = 0; x < chunk.length; x++) {
                bab.write(byte2charLUT, (chunk[x]&0xff)*2, 2);
                if(++cnt == 64) {
                    bab.write('\n');
                    cnt = 0;
                }
            }
        }        
           if(cnt > 0) bab.write('\n');
    }
    
    
    private int imageDataSize()
    {
        int size = 0;
        for(int k = 0; k < imageData.length; k++) {
            size += imageData[k].length;
        }   
        return size;
    }
    
     
    public void writeContent(final OutputStream result) throws IOException
    {
        
        if(this.topLevelElement) {
            result.write(RtfParagraph.PARAGRAPH_DEFAULTS);
            switch(alignment) {
                case Element.ALIGN_LEFT:
                    result.write(RtfParagraphStyle.ALIGN_LEFT);
                    break;
                case Element.ALIGN_RIGHT:
                    result.write(RtfParagraphStyle.ALIGN_RIGHT);
                    break;
                case Element.ALIGN_CENTER:
                    result.write(RtfParagraphStyle.ALIGN_CENTER);
                    break;
                case Element.ALIGN_JUSTIFIED:
                    result.write(RtfParagraphStyle.ALIGN_JUSTIFY);
                    break;
            }
        }
        result.write(OPEN_GROUP);
        result.write(PICTURE_GROUP);
        result.write(OPEN_GROUP);
        result.write(PICTURE);
        switch(imageType) {
            case Image.ORIGINAL_JPEG:
                result.write(PICTURE_JPEG);
                break;
            case Image.ORIGINAL_PNG:
            case Image.ORIGINAL_GIF:
                result.write(PICTURE_PNG);
                break;
            case Image.ORIGINAL_WMF:
            case Image.ORIGINAL_BMP:
                result.write(PICTURE_WMF);
                break;
        }
        result.write(PICTURE_WIDTH);
        result.write(intToByteArray((int) width));
        result.write(PICTURE_HEIGHT);
        result.write(intToByteArray((int) height));
        if(this.document.getDocumentSettings().isWriteImageScalingInformation()) {
            result.write(PICTURE_SCALE_X);
            result.write(intToByteArray((int)(100 * plainWidth / width)));
            result.write(PICTURE_SCALE_Y);
            result.write(intToByteArray((int)(100 * plainHeight / height)));
        }
        if(this.document.getDocumentSettings().isImagePDFConformance()) {
            result.write(PICTURE_SCALED_WIDTH);
            result.write(intToByteArray((int) (plainWidth * RtfElement.TWIPS_FACTOR)));
            result.write(PICTURE_SCALED_HEIGHT);
            result.write(intToByteArray((int) (plainHeight * RtfElement.TWIPS_FACTOR)));
        } else {
            if(this.width != this.plainWidth || this.imageType == Image.ORIGINAL_BMP) {
                result.write(PICTURE_SCALED_WIDTH);
                result.write(intToByteArray((int) (plainWidth * PIXEL_TWIPS_FACTOR)));
            }
            if(this.height != this.plainHeight || this.imageType == Image.ORIGINAL_BMP) {
                result.write(PICTURE_SCALED_HEIGHT);
                result.write(intToByteArray((int) (plainHeight * PIXEL_TWIPS_FACTOR)));
            }
        }

        if(this.document.getDocumentSettings().isImageWrittenAsBinary()) {
            
            result.write('\n');
            result.write(PICTURE_BINARY_DATA);
            result.write(intToByteArray(imageDataSize()));
            result.write(DELIMITER);
            if(result instanceof RtfByteArrayBuffer) {
                ((RtfByteArrayBuffer)result).append(imageData);
            } else {
                for(int k = 0; k < imageData.length; k++) {
                    result.write(imageData[k]);
                }
            }
        } else {
            
            result.write(DELIMITER);
            result.write('\n');
            writeImageDataHexEncoded(result);
        }
        
        result.write(CLOSE_GROUP);
        result.write(CLOSE_GROUP);
        if(this.topLevelElement) {
            result.write(RtfParagraph.PARAGRAPH);
            result.write(RtfParagraph.PARAGRAPH);
        }
        result.write('\n');        
    }
    
    
    public void setAlignment(int alignment) {
        this.alignment = alignment;
    }
    
    
    public void setTopLevelElement(boolean topLevelElement) {
        this.topLevelElement = topLevelElement;
    }
    
    
}
