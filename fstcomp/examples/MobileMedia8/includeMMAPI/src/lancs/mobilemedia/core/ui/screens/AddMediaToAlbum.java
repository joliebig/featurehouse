package lancs.mobilemedia.core.ui.screens;
public class AddMediaToAlbum {
  TextField itemtype=new TextField("Type of media","",20,TextField.ANY);
  public String getItemType(){
    return itemtype.getString();
  }
  protected void hook44(){
    this.append(itemtype);
    original();
  }
}
