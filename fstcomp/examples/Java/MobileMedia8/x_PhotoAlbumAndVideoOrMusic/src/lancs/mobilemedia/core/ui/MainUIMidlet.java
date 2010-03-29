package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook13() throws MIDletStateChangeException {
      selectcontroller=new SelectMediaController(_this,_this.imageModel,album);
      selectcontroller.setNextController(_this.imageRootController);
      original();
    }
  }
}
