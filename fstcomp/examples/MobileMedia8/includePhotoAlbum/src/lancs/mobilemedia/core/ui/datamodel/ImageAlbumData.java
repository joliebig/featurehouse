package lancs.mobilemedia.core.ui.datamodel;
import javax.microedition.lcdui.Image;
import lancs.mobilemedia.lib.exceptions.ImageNotFoundException;
import lancs.mobilemedia.lib.exceptions.PersistenceMechanismException;
import de.ovgu.cide.jakutil.*;
public class ImageAlbumData extends AlbumData {
  public ImageAlbumData(){
    mediaAccessor=new ImageMediaAccessor(this);
  }
  /** 
 * Get a particular image (by name) from a photo album. The album name corresponds
 * to a record store.
 * @throws ImageNotFoundException 
 * @throws PersistenceMechanismException 
 */
  public Image getImageFromRecordStore(  String recordStore,  String imageName) throws ImageNotFoundException, PersistenceMechanismException {
    MediaData imageInfo=null;
    imageInfo=mediaAccessor.getMediaInfo(imageName);
    int imageId=imageInfo.getForeignRecordId();
    String album=imageInfo.getParentAlbumName();
    Image imageRec=((ImageMediaAccessor)mediaAccessor).loadSingleImageFromRMS(album,imageId);
    return imageRec;
  }
}
