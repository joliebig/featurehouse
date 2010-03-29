package lancs.mobilemedia.core.ui.controller;
import lancs.mobilemedia.core.ui.datamodel.MusicAlbumData;
import lancs.mobilemedia.core.ui.screens.PlayMediaScreen;
public class MediaController {
  private boolean playMultiMedia(  String selectedMediaName){
    InputStream storedMusic=null;
    try {
      MediaData mymedia=getAlbumData().getMediaInfo(selectedMediaName);
      incrementCountViews(selectedMediaName);
      if ((mymedia.getTypeMedia().equals(MediaData.MUSIC)) || (mymedia.getTypeMedia().equals(MediaData.VIDEO))) {
        storedMusic=((MusicAlbumData)getAlbumData()).getMusicFromRecordStore(getCurrentStoreName(),selectedMediaName);
        PlayMediaScreen playscree=new PlayMediaScreen(midlet,storedMusic,mymedia.getTypeMedia(),this);
        MusicPlayController controller=new MusicPlayController(midlet,getAlbumData(),(AlbumListScreen)getAlbumListScreen(),playscree);
        controller.setMediaName(selectedMediaName);
        this.setNextController(controller);
      }
      return true;
    }
 catch (    ImageNotFoundException e) {
      Alert alert=new Alert("Error","The selected item was not found in the mobile device",null,AlertType.ERROR);
      Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
      return false;
    }
catch (    PersistenceMechanismException e) {
      Alert alert=new Alert("Error","The mobile database can open this item 1",null,AlertType.ERROR);
      Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
      return false;
    }
  }
@MethodObject static class MediaController_handleCommand {
    protected void hook23(){
      if (label.equals("Play")) {
        selectedMediaName=_this.getSelectedMediaName();
        throw new ReturnBoolean(_this.playMultiMedia(selectedMediaName));
      }
      original();
    }
    protected void hook24() throws InvalidImageDataException, PersistenceMechanismException, ImageNotFoundException {
      if (_this.getAlbumData() instanceof MusicAlbumData) {
        _this.getAlbumData().loadMediaDataFromRMS(_this.getCurrentStoreName());
        mymedia=_this.getAlbumData().getMediaInfo(((AddMediaToAlbum)_this.getCurrentScreen()).getItemName());
        mymedia.setTypeMedia(((AddMediaToAlbum)_this.getCurrentScreen()).getItemType());
        _this.getAlbumData().updateMediaInfo(mymedia,mymedia);
      }
      original();
    }
    protected void hook25(){
      alert=new Alert("Error","The selected item was not found in the mobile device",null,AlertType.ERROR);
      Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
      throw new ReturnBoolean(true);
      original();
    }
  }
}
