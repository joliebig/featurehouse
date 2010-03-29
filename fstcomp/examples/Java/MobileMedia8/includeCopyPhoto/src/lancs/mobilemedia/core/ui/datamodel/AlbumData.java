package lancs.mobilemedia.core.ui.datamodel;
public abstract class AlbumData {
  /** 
 * @param mediaData
 * @param albumname
 * @throws InvalidImageDataException
 * @throws PersistenceMechanismException
 */
  public void addMediaData(  MediaData mediaData,  String albumname) throws InvalidImageDataException, PersistenceMechanismException {
    mediaAccessor.addMediaData(mediaData,albumname);
  }
}
