package lancs.mobilemedia.core.ui.screens;
public class PhotoViewScreen {
  byte[] byteImage=null;
  public static final Command smscopyCommand=new Command("Send Photo by SMS",Command.ITEM,1);
  private boolean fromSMS=false;
  public void setImage(  byte[] img){
    byteImage=img;
  }
  public byte[] getImage(){
    return byteImage;
  }
  public boolean isFromSMS(){
    return fromSMS;
  }
  public void setFromSMS(  boolean fromSMS){
    this.fromSMS=fromSMS;
  }
  /** 
 * Constructor
 * @param img
 */
  PhotoViewScreen(  Image img){
    this.addCommand(smscopyCommand);
  }
}
