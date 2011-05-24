package lancs.mobilemedia.core.ui.controller;
import lancs.mobilemedia.core.ui.datamodel.VideoAlbumData;
public class MediaListController {
  protected MediaListScreen hook34(  MediaListScreen mediaList){
    if (getAlbumData() instanceof VideoAlbumData)     mediaList=new MediaListScreen(MediaListScreen.PLAYVIDEO);
    return original(mediaList);
  }
}
