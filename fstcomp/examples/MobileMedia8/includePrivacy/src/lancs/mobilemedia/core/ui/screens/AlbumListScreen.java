package lancs.mobilemedia.core.ui.screens;
public class AlbumListScreen {
  public static final Command addPassword=new Command("Add Password",Command.ITEM,1);
  protected void hook45(){
    this.addCommand(addPassword);
    original();
  }
}
