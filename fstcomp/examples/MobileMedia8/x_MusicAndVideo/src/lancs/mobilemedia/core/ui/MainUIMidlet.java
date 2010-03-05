package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook12() throws MIDletStateChangeException {
      selectcontroller2=new SelectMediaController(_this,_this.musicModel,albumMusic);
      selectcontroller2.setNextController(_this.musicRootController);
      original();
    }
  }
}
