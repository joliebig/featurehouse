package lancs.mobilemedia.core.ui.screens;
import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.Image;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.util.Constants;
import de.ovgu.cide.jakutil.*;
/** 
 * This screen displays a selected image.
 */
public class PhotoViewScreen extends Canvas {
  String imageName="";
  Image image;
  AlbumData model=null;
  public static final Command backCommand=new Command("Back",Command.BACK,0);
  /** 
 * Constructor
 * @param img
 */
  public PhotoViewScreen(  Image img){
    image=img;
    this.addCommand(backCommand);
  }
  protected void paint(  Graphics g){
    g.setGrayScale(255);
    g.fillRect(0,0,Constants.SCREEN_WIDTH,Constants.SCREEN_HEIGHT);
    System.out.println("Screen size:" + Constants.SCREEN_WIDTH + ":"+ Constants.SCREEN_HEIGHT);
    if (image == null)     System.out.println("PhotoViewScreen::paint(): Image object was null.");
    g.drawImage(image,0,0,Graphics.TOP | Graphics.LEFT);
  }
}
