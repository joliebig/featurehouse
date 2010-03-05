package lancs.mobilemedia.core.ui.controller;
import javax.microedition.lcdui.Alert;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.CommandListener;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.Displayable;
import javax.microedition.lcdui.List;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import de.ovgu.cide.jakutil.*;
/** 
 * [EF] Added in scenario 04. 
 * Purpose: (i) to structure controllers and (ii) simplify method handleCommand.
 * @author Eduardo Figueiredo
 */
public abstract class AbstractController implements CommandListener, ControllerInterface {
  protected MainUIMidlet midlet;
  private ControllerInterface nextController;
  private AlbumData albumData;
  private List albumListScreen;
  /** 
 * @param midlet
 * @param nextController
 * @param albumData
 * @param albumListScreen
 * @param currentScreenName
 */
  public AbstractController(  MainUIMidlet midlet,  AlbumData albumData,  List albumListScreen){
    this.midlet=midlet;
    this.albumData=albumData;
    this.albumListScreen=albumListScreen;
  }
  public void postCommand(  Command command){
    System.out.println("AbstractController::postCommand - Current controller is: " + this.getClass().getName());
    if (handleCommand(command) == false) {
      ControllerInterface next=getNextController();
      if (next != null) {
        System.out.println("Passing to next controller in chain: " + next.getClass().getName());
        next.postCommand(command);
      }
 else {
        System.out.println("AbstractController::postCommand - Reached top of chain. No more handlers for command: " + command);
      }
    }
  }
  public void commandAction(  Command c,  Displayable d){
    postCommand(c);
  }
  /** 
 * @param a
 */
  public void setAlbumListAsCurrentScreen(  Alert a){
    setCurrentScreen(a,albumListScreen);
  }
  /** 
 * Set the current screen for display, after alert
 */
  public void setCurrentScreen(  Alert a,  Displayable d){
    Display.getDisplay(midlet).setCurrent(a,d);
  }
  /** 
 * [EF] RENAMED in Scenario 04: remove "Name". Purpose: avoid method name conflict
 * Get the current screen name that is displayed
 */
  public Displayable getCurrentScreen(){
    return Display.getDisplay(midlet).getCurrent();
  }
  /** 
 * Set the current screen for display
 */
  public void setCurrentScreen(  Displayable d){
    Display.getDisplay(midlet).setCurrent(d);
  }
  /** 
 * @return the albumData
 */
  public AlbumData getAlbumData(){
    return albumData;
  }
  /** 
 * @param albumData the albumData to set
 */
  public void setAlbumData(  AlbumData albumData){
    this.albumData=albumData;
  }
  /** 
 * @return the nextController
 */
  public ControllerInterface getNextController(){
    return nextController;
  }
  /** 
 * @param nextController the nextController to set
 */
  public void setNextController(  ControllerInterface nextController){
    this.nextController=nextController;
  }
  /** 
 * [EF] Scenario 04: Just forward method.
 * @return the currentStoreName
 */
  public String getCurrentStoreName(){
    return ScreenSingleton.getInstance().getCurrentStoreName();
  }
  /** 
 * @return the albumListScreen
 */
  public List getAlbumListScreen(){
    return albumListScreen;
  }
}
