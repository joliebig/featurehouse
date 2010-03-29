package lancs.mobilemedia.core.ui.datamodel;
public class ImageMediaAccessor {
  public void addImageData(  String photoname,  byte[] imgdata,  String albumname) throws InvalidImageDataException, PersistenceMechanismException {
    try {
      addMediaArrayOfBytes(photoname,albumname,imgdata);
    }
 catch (    RecordStoreException e) {
      throw new PersistenceMechanismException();
    }
  }
}
