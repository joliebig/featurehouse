package lancs.mobilemedia.core.ui.datamodel;
import java.io.IOException;
import java.io.InputStream;
import javax.microedition.rms.RecordStoreException;
import lancs.mobilemedia.lib.exceptions.ImageNotFoundException;
import lancs.mobilemedia.lib.exceptions.InvalidImageDataException;
import lancs.mobilemedia.lib.exceptions.PersistenceMechanismException;
import de.ovgu.cide.jakutil.*;
public class VideoMediaAccessor extends MusicMediaAccessor {
  public VideoMediaAccessor(  AlbumData mod){
    super(mod,"vvp-","vvpi-","My Video Album");
  }
  public void resetRecordStore() throws InvalidImageDataException, PersistenceMechanismException {
    removeRecords();
    MediaData media=null;
    MediaData mmedi=null;
    InputStream is=(InputStream)this.getClass().getResourceAsStream("/images/fish.mpg");
    byte[] video=null;
    try {
      video=inputStreamToBytes(is);
    }
 catch (    IOException e1) {
      e1.printStackTrace();
    }
    System.out.println("Vai adicionar os dados");
    addVideoData("Fish",default_album_name,video);
    loadMediaDataFromRMS(default_album_name);
    try {
      media=this.getMediaInfo("Fish");
      mmedi=new MediaData(media.getForeignRecordId(),media.getParentAlbumName(),media.getMediaLabel());
      mmedi.setTypeMedia("video/mpeg");
      this.updateMediaInfo(media,mmedi);
    }
 catch (    ImageNotFoundException e) {
      e.printStackTrace();
    }
  }
  public void addVideoData(  String videoname,  String albumname,  byte[] video) throws InvalidImageDataException, PersistenceMechanismException {
    try {
      addMediaArrayOfBytes(videoname,albumname,video);
    }
 catch (    RecordStoreException e) {
      throw new PersistenceMechanismException();
    }
  }
  public byte[] inputStreamToBytes(  InputStream inputStream) throws IOException {
    String str=inputStream.toString();
    return str.getBytes();
  }
}
