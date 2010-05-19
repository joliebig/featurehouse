package lancs.mobilemedia.core.ui.controller;
public class MediaController {
@MethodObject static class MediaController_handleCommand {
    protected void hook22(){
      if (label.equals("Capture Video")) {
        playscree=new CaptureVideoScreen(midlet,CaptureVideoScreen.CAPTUREVIDEO);
        playscree.setVisibleVideo();
        controller=new VideoCaptureController(midlet,_this.getAlbumData(),(AlbumListScreen)_this.getAlbumListScreen(),playscree);
        _this.setNextController(controller);
        playscree.setCommandListener(_this);
        throw new ReturnBoolean(true);
      }
      original();
    }
  }
}
