package lancs.mobilemedia.core.ui.controller;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.List;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import de.ovgu.cide.jakutil.*;
public class SelectMediaController extends AbstractController {
  public SelectMediaController(  MainUIMidlet midlet,  AlbumData imageAlbumData,  List albumListScreen){
    super(midlet,imageAlbumData,albumListScreen);
  }
  public boolean handleCommand(  Command command){
    String label=command.getLabel();
    System.out.println("<* SelectMediaController.handleCommand() *>: " + label);
    if (label.equals("Select")) {
      List down=(List)Display.getDisplay(midlet).getCurrent();
      this.hook42(down);
      this.hook41(down);
      this.hook43(down);
      return true;
    }
    return false;
  }
  protected void hook41(  List down){
  }
  protected void hook42(  List down){
  }
  protected void hook43(  List down){
  }
}
