package lancs.mobilemedia.core.ui.datamodel;
public abstract class MediaAccessor {
  /** 
 * [EF] Add in scenario 05
 * @param photoname
 * @param imageData
 * @param albumname
 * @throws InvalidImageDataException
 * @throws PersistenceMechanismException
 */
  public void addMediaData(  MediaData mediaData,  String albumname) throws InvalidImageDataException, PersistenceMechanismException {
    try {
      mediaRS=RecordStore.openRecordStore(album_label + albumname,true);
      mediaInfoRS=RecordStore.openRecordStore(info_label + albumname,true);
      int rid2;
      rid2=mediaInfoRS.getNextRecordID();
      mediaData.setRecordId(rid2);
      byte[] data1=getByteFromMediaInfo(mediaData);
      mediaInfoRS.addRecord(data1,0,data1.length);
    }
 catch (    RecordStoreException e) {
      throw new PersistenceMechanismException();
    }
 finally {
      try {
        mediaRS.closeRecordStore();
        mediaInfoRS.closeRecordStore();
      }
 catch (      RecordStoreNotOpenException e) {
        e.printStackTrace();
      }
catch (      RecordStoreException e) {
        e.printStackTrace();
      }
    }
  }
}
