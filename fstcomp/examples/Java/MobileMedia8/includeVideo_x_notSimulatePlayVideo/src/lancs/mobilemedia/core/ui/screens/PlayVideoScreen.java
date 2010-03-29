package lancs.mobilemedia.core.ui.screens;
public class PlayVideoScreen {
  protected void hook58(  InputStream storedVideo,  String type) throws Exception {
    player=Manager.createPlayer(storedVideo,type);
    original(storedVideo,type);
  }
}
