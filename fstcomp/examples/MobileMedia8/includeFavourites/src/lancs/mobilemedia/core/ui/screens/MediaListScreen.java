package lancs.mobilemedia.core.ui.screens;
public class MediaListScreen {
  public static final Command favoriteCommand=new Command("Set Favorite",Command.ITEM,1);
  public static final Command viewFavoritesCommand=new Command("View Favorites",Command.ITEM,1);
  protected void hook50(){
    this.addCommand(favoriteCommand);
    this.addCommand(viewFavoritesCommand);
    original();
  }
}
