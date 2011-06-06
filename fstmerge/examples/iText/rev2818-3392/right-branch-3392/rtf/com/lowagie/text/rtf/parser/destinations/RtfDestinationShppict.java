
 
package com.lowagie.text.rtf.parser.destinations;

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;

import com.lowagie.text.BadElementException;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.ByteBuffer;
import com.lowagie.text.rtf.direct.RtfDirectContent;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.graphic.RtfImage;
import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;


public class RtfDestinationShppict extends RtfDestination {

    private ByteBuffer data = null;

    private StringBuffer hexChars = new StringBuffer(0);
    private StringBuffer buffer = new StringBuffer();

    
    private int pictureType = Image.ORIGINAL_NONE;





    
    
    

    
    
    
    
        
        
        
        
        
        
        
        
    private int pmmetafile = 0;
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    private Integer bitsPerPixel = new Integer(1);
    
    private Integer planes = new Integer(1);
    
    private Integer widthBytes = null;
    
    
    
    
    
    
    private Long width = null;
    
    private Long height = null;
    
    private Long desiredWidth = null;
    
    private Long desiredHeight = null;
    
    private Integer scaleX = new Integer(100);
    
    private Integer scaleY = new Integer(100);
    
    private Boolean scaled = null;
    
    private Boolean inlinePicture = Boolean.FALSE;
    
    private Boolean wordArt = Boolean.FALSE;
    
    private Integer cropTop = new Integer(0);
    
    private Integer cropBottom = new Integer(0);
    
    private Integer cropLeft = new Integer(0);
    
    private Integer cropRight = new Integer(0);
    
    
    
    private boolean bitmap = false;
    
    private int bbp = 1;
    
    
    
    
    public static final int FORMAT_HEXADECIMAL = 0;
    public static final int FORMAT_BINARY = 1;
    private int dataFormat = FORMAT_HEXADECIMAL;
    private long binaryLength = 0;
    
    private Integer unitsPerInch = null;
    
    private String tag = "";
    private static final int NORMAL = 0;
    private static final int BLIPUID = 1;
    
    private int state = NORMAL;
    
    private static final int PIXEL_TWIPS_FACTOR = 15;
    
    ByteArrayOutputStream dataOS = null;

    
    public RtfDestinationShppict() {
        super(null);
    }

    
    public RtfDestinationShppict(RtfParser parser) {
        super(parser);
    }
    
    
    public boolean closeDestination() {
        if(this.rtfParser.isImport()) {
            if(this.buffer.length()>0) {
                writeBuffer();
            }
        }
        return true;
    }
    
    public boolean handleCloseGroup() {
        this.onCloseGroup();    
        
        if(this.rtfParser.isImport()) {
            if(this.buffer.length()>0) {
                writeBuffer();
            }

            if(dataOS != null) {
                addImage();
                dataOS = null;
            }
            this.writeText("}");
            return true;
        }
        if(this.rtfParser.isConvert()) {
        }
        return true;
    }
    
    private boolean addImage() {
        Image img = null;

        try {

            img = Image.getInstance(dataOS.toByteArray());
                
            } catch (BadElementException e) {
                e.printStackTrace();
            } catch (MalformedURLException e) {
                e.printStackTrace();
            } catch (IOException e) {
                
                e.printStackTrace();
            }
            
            if(img != null) {
                
                
                FileOutputStream out =null;
                try {
                    out = new FileOutputStream("c:\\testOrig.png");

                    out.write(dataOS.toByteArray());
                    out.close();
                    out = new FileOutputStream("c:\\testNew.png");
                    out.write(img.getOriginalData());
                    out.close();
                } catch (FileNotFoundException e1) {
                    e1.printStackTrace();
                } catch (IOException e1) {
                    e1.printStackTrace();
                }

                
                
                img.scaleAbsolute(this.desiredWidth.floatValue()/PIXEL_TWIPS_FACTOR, this.desiredHeight.floatValue()/PIXEL_TWIPS_FACTOR);
                img.scaleAbsolute(this.width.floatValue()/PIXEL_TWIPS_FACTOR, this.height.floatValue()/PIXEL_TWIPS_FACTOR);
                img.scalePercent(this.scaleX.floatValue(), this.scaleY.floatValue());

                
                try {
                    if(this.rtfParser.isImport()) {
                        RtfDocument rtfDoc = this.rtfParser.getRtfDocument();
                        RtfImage rtfImage = new RtfImage(rtfDoc, img);
                        rtfDoc.add(rtfImage);
                    }
                    if(this.rtfParser.isConvert()) {
                        this.rtfParser.getDocument().add(img);
                    }
                } catch (DocumentException e) {
                    
                    e.printStackTrace();
                }
            }
            

            dataFormat = FORMAT_HEXADECIMAL;
            return true;
    }

    
    public boolean handleOpenGroup() {
        this.onOpenGroup();    
        
        if(this.rtfParser.isImport()) {
        }
        if(this.rtfParser.isConvert()) {
        }
        return true;
    }
    
    public boolean handleOpeningSubGroup() {
        if(this.rtfParser.isImport()) {
            if(this.buffer.length()>0) {
                writeBuffer();
            }
        }
        return true;
    }
    
    public boolean handleCharacter(int ch) {
        
        if(this.rtfParser.isImport()) {
            if(buffer.length() > 254)
                writeBuffer();
        }
        if(data == null) data = new ByteBuffer();
        switch(dataFormat) {
        case FORMAT_HEXADECIMAL:
            hexChars.append(ch);
            if(hexChars.length() == 2) {
                try {
                    data.append((char)Integer.parseInt(hexChars.toString() , 16));
                } catch (NumberFormatException e) {
                    e.printStackTrace();
                }
                hexChars = new StringBuffer();
            }
            break;
        case FORMAT_BINARY:
            if (dataOS == null) { 
                dataOS = new ByteArrayOutputStream();
            }
            
            dataOS.write((char)(ch));
            





            binaryLength--;
            if(binaryLength == 0) { dataFormat = FORMAT_HEXADECIMAL; }
            break;
        }
        
        return true;
    }
    public boolean handleControlWord(RtfCtrlWordData ctrlWordData) {
        boolean result = false;
        boolean skipCtrlWord = false;
        if(this.rtfParser.isImport()) {
            skipCtrlWord = true;
            if(ctrlWordData.ctrlWord.equals("shppict")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("nonshppict")) {    
                skipCtrlWord = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;
                }
            if(ctrlWordData.ctrlWord.equals("blipuid")) { skipCtrlWord = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;}
            if(ctrlWordData.ctrlWord.equals("picprop")) { skipCtrlWord = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;}
            if(ctrlWordData.ctrlWord.equals("pict")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("emfblip")) { result = true; pictureType = Image.ORIGINAL_NONE;}
            if(ctrlWordData.ctrlWord.equals("pngblip")) { result = true; pictureType = Image.ORIGINAL_PNG;}
            if(ctrlWordData.ctrlWord.equals("jepgblip")) { result = true; pictureType = Image.ORIGINAL_JPEG;}
            if(ctrlWordData.ctrlWord.equals("macpict")) { result = true; pictureType = Image.ORIGINAL_NONE;}
            if(ctrlWordData.ctrlWord.equals("pmmetafile")) { result = true; pictureType = Image.ORIGINAL_NONE;}
            if(ctrlWordData.ctrlWord.equals("wmetafile")) { result = true; pictureType = Image.ORIGINAL_WMF;}
            if(ctrlWordData.ctrlWord.equals("dibitmap")) { result = true; pictureType = Image.ORIGINAL_NONE;}
            if(ctrlWordData.ctrlWord.equals("wbitmap")) { result = true; pictureType = Image.ORIGINAL_BMP;}
            
            if(ctrlWordData.ctrlWord.equals("wbmbitspixel")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("wbmplanes")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("wbmwidthbytes")) { result = true;}
            
            if(ctrlWordData.ctrlWord.equals("picw")) { this.width = ctrlWordData.toLong(); result = true;}
            if(ctrlWordData.ctrlWord.equals("pich")) { this.height = ctrlWordData.toLong(); result = true;}
            if(ctrlWordData.ctrlWord.equals("picwgoal")) { this.desiredWidth = ctrlWordData.toLong(); result = true;}
            if(ctrlWordData.ctrlWord.equals("pichgoal")) { this.desiredHeight = ctrlWordData.toLong(); result = true;}
            if(ctrlWordData.ctrlWord.equals("picscalex")) { this.scaleX = ctrlWordData.toInteger(); result = true;}
            if(ctrlWordData.ctrlWord.equals("picscaley")) { this.scaleY = ctrlWordData.toInteger();result = true;}
            if(ctrlWordData.ctrlWord.equals("picscaled")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("picprop")) { skipCtrlWord = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;}
            if(ctrlWordData.ctrlWord.equals("defshp")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("piccropt")) { this.cropTop = ctrlWordData.toInteger(); result = true;}
            if(ctrlWordData.ctrlWord.equals("piccropb")) { this.cropBottom = ctrlWordData.toInteger(); result = true;}
            if(ctrlWordData.ctrlWord.equals("piccropl")) { this.cropLeft = ctrlWordData.toInteger(); result = true;}
            if(ctrlWordData.ctrlWord.equals("piccropr")) { this.cropRight = ctrlWordData.toInteger(); result = true;}
            
            if(ctrlWordData.ctrlWord.equals("picbmp")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("picbpp")) { result = true;}
            
            if(ctrlWordData.ctrlWord.equals("bin")) { 
                this.dataFormat = FORMAT_BINARY;
                
                this.binaryLength = ctrlWordData.longValue();
                this.rtfParser.setTokeniserStateBinary(binaryLength);
                result = true;
            }
            if(ctrlWordData.ctrlWord.equals("blipupi")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("blipuid")) { skipCtrlWord = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;}
            if(ctrlWordData.ctrlWord.equals("bliptag")) { result = true;}
        }
        if(this.rtfParser.isConvert()) {
            if(ctrlWordData.ctrlWord.equals("shppict")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("nonshppict")) { skipCtrlWord = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;}
            if(ctrlWordData.ctrlWord.equals("blipuid")) { result = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;}
            if(ctrlWordData.ctrlWord.equals("pict")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("emfblip")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("pngblip")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("jepgblip")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("macpict")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("pmmetafile")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("wmetafile")) {  skipCtrlWord = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;}
            if(ctrlWordData.ctrlWord.equals("dibitmap")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("wbitmap")) { result = true;}
            
            if(ctrlWordData.ctrlWord.equals("wbmbitspixel")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("wbmplanes")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("wbmwidthbytes")) { result = true;}
            
            if(ctrlWordData.ctrlWord.equals("picw")) { this.width = ctrlWordData.toLong(); result = true;}
            if(ctrlWordData.ctrlWord.equals("pich")) { this.height = ctrlWordData.toLong(); result = true;}
            if(ctrlWordData.ctrlWord.equals("picwgoal")) { this.desiredWidth = ctrlWordData.toLong(); result = true;}
            if(ctrlWordData.ctrlWord.equals("pichgoal")) { this.desiredHeight = ctrlWordData.toLong(); result = true;}
            if(ctrlWordData.ctrlWord.equals("picscalex")) { this.scaleX = ctrlWordData.toInteger(); result = true;}
            if(ctrlWordData.ctrlWord.equals("picscaley")) { this.scaleY = ctrlWordData.toInteger();result = true;}
            if(ctrlWordData.ctrlWord.equals("picscaled")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("picprop")) { skipCtrlWord = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;}
            if(ctrlWordData.ctrlWord.equals("defshp")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("piccropt")) { this.cropTop = ctrlWordData.toInteger(); result = true;}
            if(ctrlWordData.ctrlWord.equals("piccropb")) { this.cropBottom = ctrlWordData.toInteger(); result = true;}
            if(ctrlWordData.ctrlWord.equals("piccropl")) { this.cropLeft = ctrlWordData.toInteger(); result = true;}
            if(ctrlWordData.ctrlWord.equals("piccropr")) { this.cropRight = ctrlWordData.toInteger(); result = true;}
            
            if(ctrlWordData.ctrlWord.equals("picbmp")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("picbpp")) { result = true;}
            
            if(ctrlWordData.ctrlWord.equals("bin")) { 
                dataFormat = FORMAT_BINARY; result = true;
            }
            if(ctrlWordData.ctrlWord.equals("blipupi")) { result = true;}
            if(ctrlWordData.ctrlWord.equals("blipuid")) { skipCtrlWord = true; this.rtfParser.setTokeniserStateSkipGroup(); result = true;}
            if(ctrlWordData.ctrlWord.equals("bliptag")) { result = true;}
        
        }
        if(!skipCtrlWord) {
            switch(this.rtfParser.getConversionType()) {
            case RtfParser.TYPE_IMPORT_FULL:
                    writeBuffer();
                    writeText(ctrlWordData.toString());
                result = true;
                break;        
            case RtfParser.TYPE_IMPORT_FRAGMENT:
                    writeBuffer();
                    writeText(ctrlWordData.toString());
                result = true;
                break;
            case RtfParser.TYPE_CONVERT:
                result = true;
                break;
            default:    
                result = false;
                break;
            }
        }
        return result;
    }

    
    public void setToDefaults() {
        
        this.buffer = new StringBuffer();
        this.data = null;
        this.width = null;
        this.height = null;
        this.desiredWidth = null;
        this.desiredHeight = null;
        this.scaleX = new Integer(100);
        this.scaleY = new Integer(100);
        this.scaled = null;
        this.inlinePicture = Boolean.FALSE;
        this.wordArt = Boolean.FALSE;
        this.cropTop = new Integer(0);
        this.cropBottom = new Integer(0);
        this.cropLeft = new Integer(0);
        this.cropRight = new Integer(0);
        this.bitmap = false;
        this.bbp = 1;
        this.dataFormat = FORMAT_HEXADECIMAL;
        this.binaryLength = 0;
        this.unitsPerInch = null;
        this.tag = "";
    }

    private void writeBuffer() {
        writeText(this.buffer.toString());
        
    }
    private void writeText(String value) {
        if(this.rtfParser.getState().newGroup) {
            this.rtfParser.getRtfDocument().add(new RtfDirectContent("{"));
            this.rtfParser.getState().newGroup = false;
        }
        if(value.length() > 0) {
            this.rtfParser.getRtfDocument().add(new RtfDirectContent(value));
        }
    }

}
