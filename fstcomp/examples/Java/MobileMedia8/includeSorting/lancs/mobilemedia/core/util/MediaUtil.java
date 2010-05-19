package lancs.mobilemedia.core.util;
public class MediaUtil {
  protected String hook64(  MediaData ii,  String byteString) throws InvalidImageDataException, Exception {
    byteString=byteString.concat(DELIMITER);
    byteString=byteString.concat("" + ii.getNumberOfViews());
    return original(ii,byteString);
  }
@MethodObject static class MediaUtil_getMediaInfoFromBytes {
    protected void hook65() throws InvalidArrayFormatException, Exception {
      ii.setNumberOfViews(numberOfViews);
      original();
    }
    protected void hook66() throws InvalidArrayFormatException, Exception {
      startIndex=_this.endIndex + 1;
      _this.endIndex=iiString.indexOf(_this.DELIMITER,startIndex);
      if (_this.endIndex == -1)       _this.endIndex=iiString.length();
      numberOfViews=0;
      try {
        numberOfViews=Integer.parseInt(iiString.substring(startIndex,_this.endIndex));
      }
 catch (      RuntimeException e) {
        numberOfViews=0;
        e.printStackTrace();
      }
      original();
    }
  }
}
