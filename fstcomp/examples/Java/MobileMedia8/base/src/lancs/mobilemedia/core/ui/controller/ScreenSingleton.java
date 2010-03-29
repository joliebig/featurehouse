package lancs.mobilemedia.core.ui.controller;
import de.ovgu.cide.jakutil.*;
/** 
 * @author Eduardo Figueiredo
 * [EF] Added in scenario 04. Purpose have unique data (by now currentScreenName, currentStoreName)
 * in order to make them consistent for all controllers.
 */
public class ScreenSingleton {
  private static ScreenSingleton instance;
  private String currentScreenName;
  private String currentStoreName="My Photo Album";
  private ScreenSingleton(){
  }
  /** 
 * @return the instance
 */
  public static ScreenSingleton getInstance(){
    if (instance == null)     instance=new ScreenSingleton();
    return instance;
  }
  /** 
 * @param currentScreenName the currentScreenName to set
 */
  public void setCurrentScreenName(  String currentScreenName){
    this.currentScreenName=currentScreenName;
  }
  /** 
 * @return the currentScreenName
 */
  public String getCurrentScreenName(){
    return currentScreenName;
  }
  /** 
 * @param currentStoreName the currentStoreName to set
 */
  public void setCurrentStoreName(  String currentStoreName){
    this.currentStoreName=currentStoreName;
  }
  /** 
 * @return the currentStoreName
 */
  public String getCurrentStoreName(){
    return currentStoreName;
  }
}
