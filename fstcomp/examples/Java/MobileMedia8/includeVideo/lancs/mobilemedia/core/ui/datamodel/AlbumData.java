package lancs.mobilemedia.core.ui.datamodel;
public abstract class AlbumData {
  /** 
 * @param videoname
 * @param albumname
 * @param video
 * @throws InvalidImageDataException
 * @throws PersistenceMechanismException
 */
  public void addVideoData(  String videoname,  String albumname,  byte[] video) throws InvalidImageDataException, PersistenceMechanismException {
    if (mediaAccessor instanceof VideoMediaAccessor)     ((VideoMediaAccessor)mediaAccessor).addVideoData(videoname,albumname,video);
  }
}
