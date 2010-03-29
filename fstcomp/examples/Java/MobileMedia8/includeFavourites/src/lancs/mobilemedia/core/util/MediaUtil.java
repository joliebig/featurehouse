package lancs.mobilemedia.core.util;
public class MediaUtil {
  protected String hook61(  MediaData ii,  String byteString) throws InvalidImageDataException, Exception {
    byteString=byteString.concat(DELIMITER);
    if (ii.isFavorite())     byteString=byteString.concat("true");
 else     byteString=byteString.concat("false");
    return original(ii,byteString);
  }
@MethodObject static class MediaUtil_getMediaInfoFromBytes {
    protected void hook62() throws InvalidArrayFormatException, Exception {
      ii.setFavorite(favorite);
      original();
    }
    protected void hook63() throws InvalidArrayFormatException, Exception {
      favorite=false;
      startIndex=_this.endIndex + 1;
      _this.endIndex=iiString.indexOf(_this.DELIMITER,startIndex);
      if (_this.endIndex == -1)       _this.endIndex=iiString.length();
      favorite=(iiString.substring(startIndex,_this.endIndex)).equalsIgnoreCase("true");
      original();
    }
  }
}
