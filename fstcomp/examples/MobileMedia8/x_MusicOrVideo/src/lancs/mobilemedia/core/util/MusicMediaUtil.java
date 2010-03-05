package lancs.mobilemedia.core.util;
import lancs.mobilemedia.core.ui.datamodel.MediaData;
import lancs.mobilemedia.lib.exceptions.InvalidArrayFormatException;
import lancs.mobilemedia.lib.exceptions.InvalidImageDataException;
import de.ovgu.cide.jakutil.*;
public class MusicMediaUtil extends MediaUtil {
  public byte[] getBytesFromMediaInfo(  MediaData ii) throws InvalidImageDataException {
    try {
      byte[] mediadata=super.getBytesFromMediaInfo(ii);
      if (ii.getTypeMedia() != null) {
        if ((ii.getTypeMedia().equals(MediaData.MUSIC)) || (ii.getTypeMedia().equals(MediaData.VIDEO))) {
          String byteString=new String(mediadata);
          byteString=byteString.concat(DELIMITER);
          byteString=byteString.concat(ii.getTypeMedia());
          return byteString.getBytes();
        }
      }
      return mediadata;
    }
 catch (    Exception e) {
      throw new InvalidImageDataException("The provided data are not valid");
    }
  }
  public MediaData getMultiMediaInfoFromBytes(  byte[] bytes) throws InvalidArrayFormatException {
    MediaData mediadata=super.getMediaInfoFromBytes(bytes);
    String iiString=new String(bytes);
    int startIndex=endIndex + 1;
    if (endIndex == iiString.length())     return mediadata;
    endIndex=iiString.indexOf(DELIMITER,startIndex);
    if (endIndex == -1)     endIndex=iiString.length();
    String mediaType=iiString.substring(startIndex,endIndex);
    mediadata.setTypeMedia(mediaType);
    return mediadata;
  }
}
