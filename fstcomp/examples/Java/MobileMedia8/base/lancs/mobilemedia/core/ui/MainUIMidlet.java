package lancs.mobilemedia.core.ui;
import javax.microedition.lcdui.Display;
import javax.microedition.midlet.MIDlet;
import javax.microedition.midlet.MIDletStateChangeException;
import lancs.mobilemedia.core.ui.controller.AlbumController;
import lancs.mobilemedia.core.ui.controller.BaseController;
import lancs.mobilemedia.core.ui.controller.MediaListController;
import lancs.mobilemedia.core.ui.controller.ScreenSingleton;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.ui.screens.AlbumListScreen;
import de.ovgu.cide.jakutil.*;
public class MainUIMidlet extends MIDlet {
  /** 
 * Constructor -
 */
  public MainUIMidlet(){
  }
  /** 
 * Start the MIDlet by creating new model and controller classes, and
 * initialize them as necessary
 */
  public void startApp() throws MIDletStateChangeException {
    new MainUIMidlet_startApp(this).execute();
  }
  /** 
 * Pause the MIDlet
 * This method does nothing at the moment.
 */
  public void pauseApp(){
  }
  /** 
 * Destroy the MIDlet
 */
  public void destroyApp(  boolean unconditional){
    notifyDestroyed();
  }
@MethodObject static class MainUIMidlet_startApp {
    MainUIMidlet_startApp(    MainUIMidlet _this){
      this._this=_this;
    }
    void execute() throws MIDletStateChangeException {
      this.hook9();
      this.hook11();
      this.hook10();
      this.hook13();
      this.hook3();
      this.hook2();
      this.hook4();
      this.hook12();
      this.hook0();
      this.hook1();
      this.hook15();
      this.hook6();
      this.hook5();
      this.hook14();
      this.hook8();
      this.hook7();
    }
    protected MainUIMidlet _this;
    protected AlbumListScreen album;
    protected MediaListController photoListController;
    protected AlbumController albumController;
    protected AlbumListScreen albumMusic;
    protected MediaListController musicListController;
    protected AlbumController albumMusicController;
    protected AlbumListScreen albumVideo;
    protected MediaListController videoListController;
    protected AlbumController albumVideoController;
    protected SmsReceiverController controller;
    protected SmsReceiverThread smsR;
    protected SelectMediaController selectcontroller;
    protected SelectMediaController selectcontroller2;
    protected SelectTypeOfMedia mainscreen;
    protected void hook0() throws MIDletStateChangeException {
    }
    protected void hook1() throws MIDletStateChangeException {
    }
    protected void hook2() throws MIDletStateChangeException {
    }
    protected void hook3() throws MIDletStateChangeException {
    }
    protected void hook4() throws MIDletStateChangeException {
    }
    protected void hook5() throws MIDletStateChangeException {
    }
    protected void hook6() throws MIDletStateChangeException {
    }
    protected void hook7() throws MIDletStateChangeException {
    }
    protected void hook8() throws MIDletStateChangeException {
    }
    protected void hook9() throws MIDletStateChangeException {
    }
    protected void hook10() throws MIDletStateChangeException {
    }
    protected void hook11() throws MIDletStateChangeException {
    }
    protected void hook12() throws MIDletStateChangeException {
    }
    protected void hook13() throws MIDletStateChangeException {
    }
    protected void hook14() throws MIDletStateChangeException {
    }
    protected void hook15() throws MIDletStateChangeException {
    }
  }
}
