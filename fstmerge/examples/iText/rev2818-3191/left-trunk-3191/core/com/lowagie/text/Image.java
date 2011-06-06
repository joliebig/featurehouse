

package com.lowagie.text;

import java.awt.Graphics2D;
import java.awt.color.ICC_Profile;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;

import com.lowagie.text.pdf.PRIndirectReference;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfIndirectReference;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfOCG;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfTemplate;
import com.lowagie.text.pdf.PdfWriter;
import com.lowagie.text.pdf.RandomAccessFileOrArray;
import com.lowagie.text.pdf.codec.BmpImage;
import com.lowagie.text.pdf.codec.CCITTG4Encoder;
import com.lowagie.text.pdf.codec.GifImage;
import com.lowagie.text.pdf.codec.PngImage;
import com.lowagie.text.pdf.codec.TiffImage;



public abstract class Image extends Rectangle {

    

    
    public static final int DEFAULT = 0;

    
    public static final int RIGHT = 2;

    
    public static final int LEFT = 0;

    
    public static final int MIDDLE = 1;

    
    public static final int TEXTWRAP = 4;

    
    public static final int UNDERLYING = 8;

    
    public static final int AX = 0;

    
    public static final int AY = 1;

    
    public static final int BX = 2;

    
    public static final int BY = 3;

    
    public static final int CX = 4;

    
    public static final int CY = 5;

    
    public static final int DX = 6;

    
    public static final int DY = 7;

    
    public static final int ORIGINAL_NONE = 0;

    
    public static final int ORIGINAL_JPEG = 1;

    
    public static final int ORIGINAL_PNG = 2;

    
    public static final int ORIGINAL_GIF = 3;

    
    public static final int ORIGINAL_BMP = 4;

    
    public static final int ORIGINAL_TIFF = 5;

    
    public static final int ORIGINAL_WMF = 6;

    
    public static final int ORIGINAL_PS = 7;

    
    public static final int ORIGINAL_JPEG2000 = 8;

    

    
    protected int type;

    
    protected URL url;

    
    protected byte rawData[];

    
    protected int bpc = 1;
    
    
    protected PdfTemplate template[] = new PdfTemplate[1];

    
    protected int alignment;

    
    protected String alt;

    
    protected float absoluteX = Float.NaN;

    
    protected float absoluteY = Float.NaN;

    
    protected float plainWidth;

    
    protected float plainHeight;

    
    protected float scaledWidth;

    
    protected float scaledHeight;

    
    protected Long mySerialId = getSerialId();

    
    
    
    public Image(URL url) {
        super(0, 0);
        this.url = url;
        this.alignment = DEFAULT;
        rotationRadians = 0;
    }

    
    public static Image getInstance(URL url) throws BadElementException,
            MalformedURLException, IOException {
        InputStream is = null;
        try {
            is = url.openStream();
            int c1 = is.read();
            int c2 = is.read();
            int c3 = is.read();
            int c4 = is.read();
            is.close();

            is = null;
            if (c1 == 'G' && c2 == 'I' && c3 == 'F') {
                GifImage gif = new GifImage(url);
                Image img = gif.getImage(1);
                return img;
            }
            if (c1 == 0xFF && c2 == 0xD8) {
                return new Jpeg(url);
            }
            if (c1 == 0x00 && c2 == 0x00 && c3 == 0x00 && c4 == 0x0c) {
                return new Jpeg2000(url);
            }
            if (c1 == 0xff && c2 == 0x4f && c3 == 0xff && c4 == 0x51) {
                return new Jpeg2000(url);
            }
            if (c1 == PngImage.PNGID[0] && c2 == PngImage.PNGID[1]
                    && c3 == PngImage.PNGID[2] && c4 == PngImage.PNGID[3]) {
                return PngImage.getImage(url);
            }
            if (c1 == 0xD7 && c2 == 0xCD) {
                return new ImgWMF(url);
            }
            if (c1 == 'B' && c2 == 'M') {
                return  BmpImage.getImage(url);
            }
            if ((c1 == 'M' && c2 == 'M' && c3 == 0 && c4 == 42)
                    || (c1 == 'I' && c2 == 'I' && c3 == 42 && c4 == 0)) {
                RandomAccessFileOrArray ra = null;
                try {
                    if (url.getProtocol().equals("file")) {
                        String file = url.getFile();
                        file = Utilities.unEscapeURL(file);
                        ra = new RandomAccessFileOrArray(file);
                    } else
                        ra = new RandomAccessFileOrArray(url);
                    Image img = TiffImage.getTiffImage(ra, 1);
                    img.url = url;
                    return img;
                } finally {
                    if (ra != null)
                        ra.close();
                }

            }
            throw new IOException(url.toString()
                    + " is not a recognized imageformat.");
        } finally {
            if (is != null) {
                is.close();
            }
        }
    }

    
    public static Image getInstance(String filename)
            throws BadElementException, MalformedURLException, IOException {
        return getInstance(Utilities.toURL(filename));
    }
    
    
    public static Image getInstance(byte imgb[]) throws BadElementException,
            MalformedURLException, IOException {
        InputStream is = null;
        try {
            is = new java.io.ByteArrayInputStream(imgb);
            int c1 = is.read();
            int c2 = is.read();
            int c3 = is.read();
            int c4 = is.read();
            is.close();

            is = null;
            if (c1 == 'G' && c2 == 'I' && c3 == 'F') {
                GifImage gif = new GifImage(imgb);
                return gif.getImage(1);
            }
            if (c1 == 0xFF && c2 == 0xD8) {
                return new Jpeg(imgb);
            }
            if (c1 == 0x00 && c2 == 0x00 && c3 == 0x00 && c4 == 0x0c) {
                return new Jpeg2000(imgb);
            }
            if (c1 == 0xff && c2 == 0x4f && c3 == 0xff && c4 == 0x51) {
                return new Jpeg2000(imgb);
            }
            if (c1 == PngImage.PNGID[0] && c2 == PngImage.PNGID[1]
                    && c3 == PngImage.PNGID[2] && c4 == PngImage.PNGID[3]) {
                return PngImage.getImage(imgb);
            }
            if (c1 == 0xD7 && c2 == 0xCD) {
                return new ImgWMF(imgb);
            }
            if (c1 == 'B' && c2 == 'M') {
                return BmpImage.getImage(imgb);
            }
            if ((c1 == 'M' && c2 == 'M' && c3 == 0 && c4 == 42)
                    || (c1 == 'I' && c2 == 'I' && c3 == 42 && c4 == 0)) {
                RandomAccessFileOrArray ra = null;
                try {
                    ra = new RandomAccessFileOrArray(imgb);
                    Image img = TiffImage.getTiffImage(ra, 1);
                    if (img.getOriginalData() == null)
                        img.setOriginalData(imgb);
                    return img;
                } finally {
                    if (ra != null)
                        ra.close();
                }

            }
            throw new IOException(
                    "The byte array is not a recognized imageformat.");
        } finally {
            if (is != null) {
                is.close();
            }
        }
    }

    
    public static Image getInstance(int width, int height, int components,
            int bpc, byte data[]) throws BadElementException {
        return Image.getInstance(width, height, components, bpc, data, null);
    }

    
    public static Image getInstance(int width, int height, boolean reverseBits,
            int typeCCITT, int parameters, byte[] data)
            throws BadElementException {
        return Image.getInstance(width, height, reverseBits, typeCCITT,
                parameters, data, null);
    }

    
    public static Image getInstance(int width, int height, boolean reverseBits,
            int typeCCITT, int parameters, byte[] data, int transparency[])
            throws BadElementException {
        if (transparency != null && transparency.length != 2)
            throw new BadElementException(
                    "Transparency length must be equal to 2 with CCITT images");
        Image img = new ImgCCITT(width, height, reverseBits, typeCCITT,
                parameters, data);
        img.transparency = transparency;
        return img;
    }

    
    public static Image getInstance(int width, int height, int components,
            int bpc, byte data[], int transparency[])
            throws BadElementException {
        if (transparency != null && transparency.length != components * 2)
            throw new BadElementException(
                    "Transparency length must be equal to (componentes * 2)");
        if (components == 1 && bpc == 1) {
            byte g4[] = CCITTG4Encoder.compress(data, width, height);
            return Image.getInstance(width, height, false, Image.CCITTG4,
                    Image.CCITT_BLACKIS1, g4, transparency);
        }
        Image img = new ImgRaw(width, height, components, bpc, data);
        img.transparency = transparency;
        return img;
    }

    
    
    
    public static Image getInstance(PdfTemplate template)
            throws BadElementException {
        return new ImgTemplate(template);
    }
    
    
    
    
    public static Image getInstance(java.awt.Image image, java.awt.Color color,
            boolean forceBW) throws BadElementException, IOException {
        
        if(image instanceof BufferedImage){
            BufferedImage bi = (BufferedImage) image;
            if(bi.getType()==BufferedImage.TYPE_BYTE_BINARY) {
                forceBW=true;
            }
        }
        
        java.awt.image.PixelGrabber pg = new java.awt.image.PixelGrabber(image,
                0, 0, -1, -1, true);
        try {
            pg.grabPixels();
        } catch (InterruptedException e) {
            throw new IOException(
                    "java.awt.Image Interrupted waiting for pixels!");
        }
        if ((pg.getStatus() & java.awt.image.ImageObserver.ABORT) != 0) {
            throw new IOException("java.awt.Image fetch aborted or errored");
        }
        int w = pg.getWidth();
        int h = pg.getHeight();
        int[] pixels = (int[]) pg.getPixels();
        if (forceBW) {
            int byteWidth = (w / 8) + ((w & 7) != 0 ? 1 : 0);
            byte[] pixelsByte = new byte[byteWidth * h];

            int index = 0;
            int size = h * w;
            int transColor = 1;
            if (color != null) {
                transColor = (color.getRed() + color.getGreen()
                        + color.getBlue() < 384) ? 0 : 1;
            }
            int transparency[] = null;
            int cbyte = 0x80;
            int wMarker = 0;
            int currByte = 0;
            if (color != null) {
                for (int j = 0; j < size; j++) {
                    int alpha = (pixels[j] >> 24) & 0xff;
                    if (alpha < 250) {
                        if (transColor == 1)
                            currByte |= cbyte;
                    } else {
                        if ((pixels[j] & 0x888) != 0)
                            currByte |= cbyte;
                    }
                    cbyte >>= 1;
                    if (cbyte == 0 || wMarker + 1 >= w) {
                        pixelsByte[index++] = (byte) currByte;
                        cbyte = 0x80;
                        currByte = 0;
                    }
                    ++wMarker;
                    if (wMarker >= w)
                        wMarker = 0;
                }
            } else {
                for (int j = 0; j < size; j++) {
                    if (transparency == null) {
                        int alpha = (pixels[j] >> 24) & 0xff;
                        if (alpha == 0) {
                            transparency = new int[2];
                            transparency[0] = transparency[1] = ((pixels[j] & 0x888) != 0) ? 1
                                    : 0;
                        }
                    }
                    if ((pixels[j] & 0x888) != 0)
                        currByte |= cbyte;
                    cbyte >>= 1;
                    if (cbyte == 0 || wMarker + 1 >= w) {
                        pixelsByte[index++] = (byte) currByte;
                        cbyte = 0x80;
                        currByte = 0;
                    }
                    ++wMarker;
                    if (wMarker >= w)
                        wMarker = 0;
                }
            }
            return Image.getInstance(w, h, 1, 1, pixelsByte, transparency);
        } else {
            byte[] pixelsByte = new byte[w * h * 3];
            byte[] smask = null;

            int index = 0;
            int size = h * w;
            int red = 255;
            int green = 255;
            int blue = 255;
            if (color != null) {
                red = color.getRed();
                green = color.getGreen();
                blue = color.getBlue();
            }
            int transparency[] = null;
            if (color != null) {
                for (int j = 0; j < size; j++) {
                    int alpha = (pixels[j] >> 24) & 0xff;
                    if (alpha < 250) {
                        pixelsByte[index++] = (byte) red;
                        pixelsByte[index++] = (byte) green;
                        pixelsByte[index++] = (byte) blue;
                    } else {
                        pixelsByte[index++] = (byte) ((pixels[j] >> 16) & 0xff);
                        pixelsByte[index++] = (byte) ((pixels[j] >> 8) & 0xff);
                        pixelsByte[index++] = (byte) ((pixels[j]) & 0xff);
                    }
                }
            } else {
                int transparentPixel = 0;
                smask = new byte[w * h];
                boolean shades = false;
                for (int j = 0; j < size; j++) {
                    byte alpha = smask[j] = (byte) ((pixels[j] >> 24) & 0xff);
                    
                    if (!shades) {
                        if (alpha != 0 && alpha != -1) {
                            shades = true;
                        } else if (transparency == null) {
                            if (alpha == 0) {
                                transparentPixel = pixels[j] & 0xffffff;
                                transparency = new int[6];
                                transparency[0] = transparency[1] = (transparentPixel >> 16) & 0xff;
                                transparency[2] = transparency[3] = (transparentPixel >> 8) & 0xff;
                                transparency[4] = transparency[5] = transparentPixel & 0xff;
                            }
                        } else if ((pixels[j] & 0xffffff) != transparentPixel) {
                            shades = true;
                        }
                    }
                    pixelsByte[index++] = (byte) ((pixels[j] >> 16) & 0xff);
                    pixelsByte[index++] = (byte) ((pixels[j] >> 8) & 0xff);
                    pixelsByte[index++] = (byte) ((pixels[j]) & 0xff);
                }
                if (shades)
                    transparency = null;
                else
                    smask = null;
            }
            Image img = Image.getInstance(w, h, 3, 8, pixelsByte, transparency);
            if (smask != null) {
                Image sm = Image.getInstance(w, h, 1, 8, smask);
                try {
                    sm.makeMask();
                    img.setImageMask(sm);
                } catch (DocumentException de) {
                    throw new ExceptionConverter(de);
                }
            }
            return img;
        }
    }

    
    public static Image getInstance(java.awt.Image image, java.awt.Color color)
            throws BadElementException, IOException {
        return Image.getInstance(image, color, false);
    }
    
    
    public static Image getInstance(PdfWriter writer, java.awt.Image awtImage, float quality) throws BadElementException, IOException {
        return getInstance(new PdfContentByte(writer), awtImage, quality);
    }
    
    
    public static Image getInstance(PdfContentByte cb, java.awt.Image awtImage, float quality) throws BadElementException, IOException {
        java.awt.image.PixelGrabber pg = new java.awt.image.PixelGrabber(awtImage,
                0, 0, -1, -1, true);
        try {
            pg.grabPixels();
        } catch (InterruptedException e) {
            throw new IOException(
                    "java.awt.Image Interrupted waiting for pixels!");
        }
        if ((pg.getStatus() & java.awt.image.ImageObserver.ABORT) != 0) {
            throw new IOException("java.awt.Image fetch aborted or errored");
        }
        int w = pg.getWidth();
        int h = pg.getHeight();
        PdfTemplate tp = cb.createTemplate(w, h);
        Graphics2D g2d = tp.createGraphics(w, h, true, quality);
        g2d.drawImage(awtImage, 0, 0, null);
        g2d.dispose();
        return getInstance(tp);
    }

    
    
    
    private PdfIndirectReference directReference;
    
    
    public PdfIndirectReference getDirectReference() {
        return this.directReference;
    }
    
    
    public void setDirectReference(PdfIndirectReference directReference) {
        this.directReference = directReference;
    }
    
        
    public static Image getInstance(PRIndirectReference ref) throws BadElementException {
        PdfDictionary dic = (PdfDictionary)PdfReader.getPdfObjectRelease(ref);
        int width = ((PdfNumber)PdfReader.getPdfObjectRelease(dic.get(PdfName.WIDTH))).intValue();
        int height = ((PdfNumber)PdfReader.getPdfObjectRelease(dic.get(PdfName.HEIGHT))).intValue();
        Image imask = null;
        PdfObject obj = dic.get(PdfName.SMASK);
        if (obj != null && obj.isIndirect()) {
            imask = getInstance((PRIndirectReference)obj);
        }
        else {
            obj = dic.get(PdfName.MASK);
            if (obj != null && obj.isIndirect()) {
                PdfObject obj2 = PdfReader.getPdfObjectRelease(obj);
                if (obj2 instanceof PdfDictionary)
                    imask = getInstance((PRIndirectReference)obj);
            }
        }
        Image img = new ImgRaw(width, height, 1, 1, null);
        img.imageMask = imask;
        img.directReference = ref;
        return img;
    }

    
    
    
    protected Image(Image image) {
        super(image);
        this.type = image.type;
        this.url = image.url;
        this.rawData = image.rawData;
        this.bpc = image.bpc;
        this.template = image.template;
        this.alignment = image.alignment;
        this.alt = image.alt;
        this.absoluteX = image.absoluteX;
        this.absoluteY = image.absoluteY;
        this.plainWidth = image.plainWidth;
        this.plainHeight = image.plainHeight;
        this.scaledWidth = image.scaledWidth;
        this.scaledHeight = image.scaledHeight;
        this.mySerialId = image.mySerialId;

        this.directReference = image.directReference;
        
        this.rotationRadians = image.rotationRadians;
        this.initialRotation = image.initialRotation;
        this.indentationLeft = image.indentationLeft;
        this.indentationRight = image.indentationRight;
        this.spacingBefore = image.spacingBefore;
        this.spacingAfter = image.spacingAfter;

        this.widthPercentage = image.widthPercentage;
        this.annotation = image.annotation;
        this.layer = image.layer;
        this.interpolation = image.interpolation;
        this.originalType = image.originalType;
        this.originalData = image.originalData;
        this.deflated = image.deflated;
        this.dpiX = image.dpiX;
        this.dpiY = image.dpiY;
        this.XYRatio = image.XYRatio;
        
        this.colorspace = image.colorspace;
        this.invert = image.invert;
        this.profile = image.profile;
        this.additional = image.additional;
        this.mask = image.mask;
        this.imageMask = image.imageMask;
        this.smask = image.smask;
        this.transparency = image.transparency;
    }

    
    public static Image getInstance(Image image) {
        if (image == null)
            return null;
        try {
            Class cs = image.getClass();
            Constructor constructor = cs
                    .getDeclaredConstructor(new Class[] { Image.class });
            return (Image) constructor.newInstance(new Object[] { image });
        } catch (Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    
    
    

    public int type() {
        return type;
    }

    
    public boolean isNestable() {
        return true;
    }

    

    

    public boolean isJpeg() {
        return type == JPEG;
    }

    

    public boolean isImgRaw() {
        return type == IMGRAW;
    }

    

    public boolean isImgTemplate() {
        return type == IMGTEMPLATE;
    }
    
    

    

    public URL getUrl() {
        return url;
    }

    
    public void setUrl(URL url) {
        this.url = url;
    }

    
    public byte[] getRawData() {
        return rawData;
    }

    
    public int getBpc() {
        return bpc;
    }

    
    public PdfTemplate getTemplateData() {
        return template[0];
    }

    
    public void setTemplateData(PdfTemplate template) {
        this.template[0] = template;
    }

    
    public int getAlignment() {
        return alignment;
    }

    

    public void setAlignment(int alignment) {
        this.alignment = alignment;
    }

    

    public String getAlt() {
        return alt;
    }

    

    public void setAlt(String alt) {
        this.alt = alt;
    }

    

    public void setAbsolutePosition(float absoluteX, float absoluteY) {
        this.absoluteX = absoluteX;
        this.absoluteY = absoluteY;
    }

    
    public boolean hasAbsoluteX() {
        return !Float.isNaN(absoluteX);
    }

    
    public float getAbsoluteX() {
        return absoluteX;
    }

    
    public boolean hasAbsoluteY() {
        return !Float.isNaN(absoluteY);
    }

    
    public float getAbsoluteY() {
        return absoluteY;
    }

    

    
    public float getScaledWidth() {
        return scaledWidth;
    }

    
    public float getScaledHeight() {
        return scaledHeight;
    }

    
    public float getPlainWidth() {
        return plainWidth;
    }

    
    public float getPlainHeight() {
        return plainHeight;
    }
    
    
    public void scaleAbsolute(float newWidth, float newHeight) {
        plainWidth = newWidth;
        plainHeight = newHeight;
        float[] matrix = matrix();
        scaledWidth = matrix[DX] - matrix[CX];
        scaledHeight = matrix[DY] - matrix[CY];
    }

    
    public void scaleAbsoluteWidth(float newWidth) {
        plainWidth = newWidth;
        float[] matrix = matrix();
        scaledWidth = matrix[DX] - matrix[CX];
        scaledHeight = matrix[DY] - matrix[CY];
    }

    
    public void scaleAbsoluteHeight(float newHeight) {
        plainHeight = newHeight;
        float[] matrix = matrix();
        scaledWidth = matrix[DX] - matrix[CX];
        scaledHeight = matrix[DY] - matrix[CY];
    }

    
    public void scalePercent(float percent) {
        scalePercent(percent, percent);
    }

    
    public void scalePercent(float percentX, float percentY) {
        plainWidth = (getWidth() * percentX) / 100f;
        plainHeight = (getHeight() * percentY) / 100f;
        float[] matrix = matrix();
        scaledWidth = matrix[DX] - matrix[CX];
        scaledHeight = matrix[DY] - matrix[CY];
    }

    
    public void scaleToFit(float fitWidth, float fitHeight) {
        scalePercent(100);
        float percentX = (fitWidth * 100) / getScaledWidth();
        float percentY = (fitHeight * 100) / getScaledHeight();
        scalePercent(percentX < percentY ? percentX : percentY);
    }

    
    public float[] matrix() {
        float[] matrix = new float[8];
        float cosX = (float) Math.cos(rotationRadians);
        float sinX = (float) Math.sin(rotationRadians);
        matrix[AX] = plainWidth * cosX;
        matrix[AY] = plainWidth * sinX;
        matrix[BX] = (-plainHeight) * sinX;
        matrix[BY] = plainHeight * cosX;
        if (rotationRadians < Math.PI / 2f) {
            matrix[CX] = matrix[BX];
            matrix[CY] = 0;
            matrix[DX] = matrix[AX];
            matrix[DY] = matrix[AY] + matrix[BY];
        } else if (rotationRadians < Math.PI) {
            matrix[CX] = matrix[AX] + matrix[BX];
            matrix[CY] = matrix[BY];
            matrix[DX] = 0;
            matrix[DY] = matrix[AY];
        } else if (rotationRadians < Math.PI * 1.5f) {
            matrix[CX] = matrix[AX];
            matrix[CY] = matrix[AY] + matrix[BY];
            matrix[DX] = matrix[BX];
            matrix[DY] = 0;
        } else {
            matrix[CX] = 0;
            matrix[CY] = matrix[AY];
            matrix[DX] = matrix[AX] + matrix[BX];
            matrix[DY] = matrix[BY];
        }
        return matrix;
    }

    

    
    static long serialId = 0;
    
    
    static protected synchronized Long getSerialId() {
        ++serialId;
        return new Long(serialId);
    }

    
    public Long getMySerialId() {
        return mySerialId;
    }

    

    
    protected float rotationRadians;
    
    
    private float initialRotation;

    
    public float getImageRotation() {
        double d = 2.0 * Math.PI;
        float rot = (float) ((rotationRadians - initialRotation) % d);
        if (rot < 0) {
            rot += d;
        }
        return rot;
    }
    
    
    public void setRotation(float r) {
        double d = 2.0 * Math.PI;
        rotationRadians = (float) ((r + initialRotation) % d);
        if (rotationRadians < 0) {
            rotationRadians += d;
        }
        float[] matrix = matrix();
        scaledWidth = matrix[DX] - matrix[CX];
        scaledHeight = matrix[DY] - matrix[CY];
    }

    
    public void setRotationDegrees(float deg) {
        double d = Math.PI;
        setRotation(deg / 180 * (float) d);
    }
    
    
    public float getInitialRotation() {
        return this.initialRotation;
    }
    
    
    public void setInitialRotation(float initialRotation) {
        float old_rot = rotationRadians - this.initialRotation;
        this.initialRotation = initialRotation;
        setRotation(old_rot);
    }
    
    

    
    protected float indentationLeft = 0;

    
    protected float indentationRight = 0;

    
    protected float spacingBefore;

    
    protected float spacingAfter;

    
    public float getIndentationLeft() {
        return indentationLeft;
    }

    
    public void setIndentationLeft(float f) {
        indentationLeft = f;
    }

    
    public float getIndentationRight() {
        return indentationRight;
    }

    
    public void setIndentationRight(float f) {
        indentationRight = f;
    }

    
    public float getSpacingBefore() {
        return spacingBefore;
    }

    

    public void setSpacingBefore(float spacing) {
        this.spacingBefore = spacing;
    }

    
    public float getSpacingAfter() {
        return spacingAfter;
    }

    

    public void setSpacingAfter(float spacing) {
        this.spacingAfter = spacing;
    }

    

    
    private float widthPercentage = 100;
    
    
    public float getWidthPercentage() {
        return this.widthPercentage;
    }

    
    public void setWidthPercentage(float widthPercentage) {
        this.widthPercentage = widthPercentage;
    }

    

    
    protected Annotation annotation = null;
    
    
    public void setAnnotation(Annotation annotation) {
        this.annotation = annotation;
    }

    
    public Annotation getAnnotation() {
        return annotation;
    }

    

    
    protected PdfOCG layer;
    
    
    public PdfOCG getLayer() {
        return layer;
    }

    
    public void setLayer(PdfOCG layer) {
        this.layer = layer;
    }

    

    
    protected boolean interpolation;

    
    public boolean isInterpolation() {
        return interpolation;
    }

    
    public void setInterpolation(boolean interpolation) {
        this.interpolation = interpolation;
    }
    
    

    
    protected int originalType = ORIGINAL_NONE;

    
    protected byte[] originalData;
    
    
    public int getOriginalType() {
        return this.originalType;
    }

    
    public void setOriginalType(int originalType) {
        this.originalType = originalType;
    }

    
    public byte[] getOriginalData() {
        return this.originalData;
    }

    
    public void setOriginalData(byte[] originalData) {
        this.originalData = originalData;
    }

    
    
    
    protected boolean deflated = false;

    
    public boolean isDeflated() {
        return this.deflated;
    }

    
    public void setDeflated(boolean deflated) {
        this.deflated = deflated;
    }
    
    
    
    
    protected int dpiX = 0;

    
    protected int dpiY = 0;

    
    public int getDpiX() {
        return dpiX;
    }

    
    public int getDpiY() {
        return dpiY;
    }

    
    public void setDpi(int dpiX, int dpiY) {
        this.dpiX = dpiX;
        this.dpiY = dpiY;
    }
    
    
    
    
    private float XYRatio = 0;

    
    public float getXYRatio() {
        return this.XYRatio;
    }

    
    public void setXYRatio(float XYRatio) {
        this.XYRatio = XYRatio;
    }
    
    

    
    protected int colorspace = -1;

    
    public int getColorspace() {
        return colorspace;
    }
    
    
    protected boolean invert = false;

    
    public boolean isInverted() {
        return invert;
    }

    
    public void setInverted(boolean invert) {
        this.invert = invert;
    }

    
    protected ICC_Profile profile = null;

    
    public void tagICC(ICC_Profile profile) {
        this.profile = profile;
    }

    
    public boolean hasICCProfile() {
        return (this.profile != null);
    }

    
    public ICC_Profile getICCProfile() {
        return profile;
    }

    
    private PdfDictionary additional = null;
    
    
    public PdfDictionary getAdditional() {
        return this.additional;
    }

    
    public void setAdditional(PdfDictionary additional) {
        this.additional = additional;
    }

        
    public void simplifyColorspace() {
        if (additional == null)
            return;
        PdfObject value = additional.get(PdfName.COLORSPACE);
        if (value == null || !value.isArray())
            return;
        PdfObject cs = simplifyColorspace(value);
        if (cs.isName())
            value = cs;
        else {
            PdfObject first = (PdfObject)(((PdfArray)value).getArrayList().get(0));
            if (PdfName.INDEXED.equals(first)) {
                ArrayList array = ((PdfArray)value).getArrayList();
                if (array.size() >= 2 && ((PdfObject)array.get(1)).isArray()) {
                     array.set(1, simplifyColorspace((PdfObject)array.get(1)));
                }
            }
        }
        additional.put(PdfName.COLORSPACE, value);
    }
    
    
    private PdfObject simplifyColorspace(PdfObject obj) {
        if (obj == null || !obj.isArray())
            return obj;
        PdfObject first = (PdfObject)(((PdfArray)obj).getArrayList().get(0));
        if (PdfName.CALGRAY.equals(first))
            return PdfName.DEVICEGRAY;
        else if (PdfName.CALRGB.equals(first))
            return PdfName.DEVICERGB;
        else
            return obj;
    }

    
    protected boolean mask = false;
    
    
    protected Image imageMask;

    
    private boolean smask;

    
    public boolean isMask() {
        return mask;
    }

    
    public void makeMask() throws DocumentException {
        if (!isMaskCandidate())
            throw new DocumentException("This image can not be an image mask.");
        mask = true;
    }

    
    public boolean isMaskCandidate() {
        if (type == IMGRAW) {
            if (bpc > 0xff)
                return true;
        }
        return colorspace == 1;
    }

    
    public Image getImageMask() {
        return imageMask;
    }

    
    public void setImageMask(Image mask) throws DocumentException {
        if (this.mask)
            throw new DocumentException(
                    "An image mask cannot contain another image mask.");
        if (!mask.mask)
            throw new DocumentException(
                    "The image mask is not a mask. Did you do makeMask()?");
        imageMask = mask;
        smask = (mask.bpc > 1 && mask.bpc <= 8);
    }

    
    public boolean isSmask() {
        return this.smask;
    }

    
    public void setSmask(boolean smask) {
        this.smask = smask;
    }

    
    protected int transparency[];

    

    public int[] getTransparency() {
        return transparency;
    }

    
    public void setTransparency(int transparency[]) {
        this.transparency = transparency;
    }

}