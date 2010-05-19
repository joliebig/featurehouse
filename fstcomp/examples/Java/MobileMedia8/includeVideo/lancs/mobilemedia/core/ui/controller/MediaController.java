package lancs.mobilemedia.core.ui.controller;
import lancs.mobilemedia.core.ui.datamodel.VideoAlbumData;
import lancs.mobilemedia.core.ui.screens.PlayVideoScreen;
public class MediaController {
  private boolean playVideoMedia(  String selectedMediaName){
    InputStream storedMusic=null;
    try {
      MediaData mymedia=getAlbumData().getMediaInfo(selectedMediaName);
      incrementCountViews(selectedMediaName);
      if ((mymedia.getTypeMedia().equals(MediaData.MUSIC)) || (mymedia.getTypeMedia().equals(MediaData.VIDEO))) {
        storedMusic=((VideoAlbumData)getAlbumData()).getVideoFromRecordStore(getCurrentStoreName(),selectedMediaName);
        PlayVideoScreen playscree=new PlayVideoScreen(midlet,storedMusic,mymedia.getTypeMedia(),this);
        playscree.setVisibleVideo();
        PlayVideoController controller=new PlayVideoController(midlet,getAlbumData(),(AlbumListScreen)getAlbumListScreen(),playscree);
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
    protected void hook28(){
      if (label.equals("Play Video")) {
        selectedMediaName=_this.getSelectedMediaName();
        throw new ReturnBoolean(_this.playVideoMedia(selectedMediaName));
      }
      original();
    }
  }
}
