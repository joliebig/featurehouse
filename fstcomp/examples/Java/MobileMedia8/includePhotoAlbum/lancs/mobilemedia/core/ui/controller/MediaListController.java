package lancs.mobilemedia.core.ui.controller;
import lancs.mobilemedia.core.ui.datamodel.ImageAlbumData;
public class MediaListController {
  protected MediaListScreen hook30(  MediaListScreen mediaList){
    if (getAlbumData() instanceof ImageAlbumData)     mediaList=new MediaListScreen(MediaListScreen.SHOWPHOTO);
    return original(mediaList);
  }
}
