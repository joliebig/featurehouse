package lancs.mobilemedia.core.ui.screens;
public class PlayVideoScreen {
  public static final Command copy=new Command("Copy",Command.ITEM,1);
  protected void hook56(){
    this.addCommand(copy);
    original();
  }
}
