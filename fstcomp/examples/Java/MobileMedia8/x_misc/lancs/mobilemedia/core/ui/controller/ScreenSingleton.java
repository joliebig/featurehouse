package lancs.mobilemedia.core.ui.controller;
import lancs.mobilemedia.core.ui.screens.SelectTypeOfMedia;
public class ScreenSingleton {
  private SelectTypeOfMedia mainscreen;
  public SelectTypeOfMedia getMainMenu(){
    return mainscreen;
  }
  public void setMainMenu(  SelectTypeOfMedia screen){
    mainscreen=screen;
  }
}
