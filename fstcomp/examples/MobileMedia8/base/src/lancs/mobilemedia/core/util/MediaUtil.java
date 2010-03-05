package lancs.mobilemedia.core.util;
import java.io.IOException;
import java.io.InputStream;
import lancs.mobilemedia.core.ui.datamodel.MediaData;
import lancs.mobilemedia.lib.exceptions.ImagePathNotValidException;
import lancs.mobilemedia.lib.exceptions.InvalidArrayFormatException;
import lancs.mobilemedia.lib.exceptions.InvalidImageDataException;
import lancs.mobilemedia.lib.exceptions.InvalidImageFormatException;
import de.ovgu.cide.jakutil.*;
/** 
 * @author trevor This is a utility class. It performs conversions between Image
 * objects and byte arrays, and Image metadata objects and byte arrays.
 * Byte arrays are the main format for storing data in RMS, and for
 * sending data over the wire.
 */
public class MediaUtil {
  protected static final String DELIMITER="*";
  protected int endIndex=0;
  /** 
 * This method reads an Image from an Input Stream and converts it from a
 * standard image file format into a byte array, so that it can be
 * transported over wireless protocols such as SMS
 * @throws ImagePathNotValidException
 * @throws InvalidImageFormatException
 */
  public byte[] readMediaAsByteArray(  String mediaFile) throws ImagePathNotValidException, InvalidImageFormatException {
    byte bArray[]=new byte[1000];
    System.out.println("<* MediaUtil.readMediaAsByteArray() *> mediaFile = " + mediaFile);
    InputStream is=null;
    try {
      is=(InputStream)this.getClass().getResourceAsStream(mediaFile);
    }
 catch (    Exception e) {
      throw new ImagePathNotValidException("Path not valid for this media:" + mediaFile);
    }
    int i, len=0;
    System.out.println("<* MediaUtil.readMediaAsByteArray() *> is = " + is);
    byte bArray2[];
    byte b[]=new byte[1];
    try {
      while (is.read(b) != -1) {
        if (len + 1 >= bArray.length) {
          bArray2=new byte[bArray.length];
          for (i=0; i < len; i++)           bArray2[i]=bArray[i];
          bArray=new byte[bArray2.length + 500];
          for (i=0; i < len; i++)           bArray[i]=bArray2[i];
        }
        bArray[len]=b[0];
        len++;
      }
      is.close();
    }
 catch (    IOException e1) {
      throw new InvalidImageFormatException("The file " + mediaFile + " does not have a correct format");
    }
catch (    NullPointerException e2) {
      throw new ImagePathNotValidException("Path not valid for this file:" + mediaFile);
    }
    return bArray;
  }
  /** 
 * Convert the byte array from a retrieved RecordStore record into the
 * ImageInfo ((renamed ImageData) object Order of the string will look like
 * this: <recordId>*<foreignRecordId>*<labelName>*<imageLabel> Depending
 * on the optional features, additional fields may be: <phoneNum>
 * @throws InvalidArrayFormatException
 */
  public MediaData getMediaInfoFromBytes(  byte[] bytes) throws InvalidArrayFormatException {
    return new MediaUtil_getMediaInfoFromBytes(this,bytes).execute();
  }
  /** 
 * Convert the ImageInfo (renamed ImageData) object into bytes so we can
 * store it in RMS Order of the string will look like this: <recordId>*<foreignRecordId>*<labelName>*<imageLabel>
 * Depending on the optional features, additional fields may be: <phoneNum>
 * @throws InvalidImageDataException 
 */
  public byte[] getBytesFromMediaInfo(  MediaData ii) throws InvalidImageDataException {
    try {
      String byteString=new String();
      int i=ii.getRecordId();
      Integer j=new Integer(i);
      byteString=byteString.concat(j.toString());
      byteString=byteString.concat(DELIMITER);
      int i2=ii.getForeignRecordId();
      Integer j2=new Integer(i2);
      byteString=byteString.concat(j2.toString());
      byteString=byteString.concat(DELIMITER);
      byteString=byteString.concat(ii.getParentAlbumName());
      byteString=byteString.concat(DELIMITER);
      byteString=byteString.concat(ii.getMediaLabel());
      byteString=this.hook61(ii,byteString);
      byteString=this.hook64(ii,byteString);
      return byteString.getBytes();
    }
 catch (    Exception e) {
      throw new InvalidImageDataException("The provided data are not valid");
    }
  }
@MethodObject static class MediaUtil_getMediaInfoFromBytes {
    MediaUtil_getMediaInfoFromBytes(    MediaUtil _this,    byte[] bytes){
      this._this=_this;
      this.bytes=bytes;
    }
    MediaData execute() throws InvalidArrayFormatException {
      try {
        iiString=new String(bytes);
        startIndex=0;
        _this.endIndex=iiString.indexOf(_this.DELIMITER);
        intString=iiString.substring(startIndex,_this.endIndex);
        startIndex=_this.endIndex + 1;
        _this.endIndex=iiString.indexOf(_this.DELIMITER,startIndex);
        fidString=iiString.substring(startIndex,_this.endIndex);
        startIndex=_this.endIndex + 1;
        _this.endIndex=iiString.indexOf(_this.DELIMITER,startIndex);
        albumLabel=iiString.substring(startIndex,_this.endIndex);
        startIndex=_this.endIndex + 1;
        _this.endIndex=iiString.indexOf(_this.DELIMITER,startIndex);
        if (_this.endIndex == -1)         _this.endIndex=iiString.length();
        imageLabel="";
        imageLabel=iiString.substring(startIndex,_this.endIndex);
        this.hook63();
        this.hook66();
        x=Integer.valueOf(fidString);
        ii=new MediaData(x.intValue(),albumLabel,imageLabel);
        this.hook62();
        this.hook65();
        x=Integer.valueOf(intString);
        ii.setRecordId(x.intValue());
        return ii;
      }
 catch (      Exception e) {
        throw new InvalidArrayFormatException();
      }
    }
    protected MediaUtil _this;
    protected byte[] bytes;
    protected String iiString;
    protected int startIndex;
    protected String intString;
    protected String fidString;
    protected String albumLabel;
    protected String imageLabel;
    protected boolean favorite;
    protected int numberOfViews;
    protected Integer x;
    protected MediaData ii;
    protected void hook62() throws InvalidArrayFormatException, Exception {
    }
    protected void hook63() throws InvalidArrayFormatException, Exception {
    }
    protected void hook65() throws InvalidArrayFormatException, Exception {
    }
    protected void hook66() throws InvalidArrayFormatException, Exception {
    }
  }
  protected String hook61(  MediaData ii,  String byteString) throws InvalidImageDataException, Exception {
    return byteString;
  }
  protected String hook64(  MediaData ii,  String byteString) throws InvalidImageDataException, Exception {
    return byteString;
  }
}
