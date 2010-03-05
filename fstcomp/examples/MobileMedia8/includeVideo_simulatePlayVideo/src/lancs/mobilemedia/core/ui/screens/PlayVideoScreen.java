package lancs.mobilemedia.core.ui.screens;
public class PlayVideoScreen {
  protected void hook57() throws Exception {
    player=Manager.createPlayer(getClass().getResourceAsStream("/images/fish.mpg"),"video/mpeg");
    original();
  }
}
