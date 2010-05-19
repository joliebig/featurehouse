package lancs.mobilemedia.core.ui.controller;
import lancs.mobilemedia.core.ui.datamodel.MusicAlbumData;
public class MediaListController {
  protected MediaListScreen hook29(  MediaListScreen mediaList){
    if (getAlbumData() instanceof MusicAlbumData)     mediaList=new MediaListScreen(MediaListScreen.PLAYMUSIC);
    return original(mediaList);
  }
}
