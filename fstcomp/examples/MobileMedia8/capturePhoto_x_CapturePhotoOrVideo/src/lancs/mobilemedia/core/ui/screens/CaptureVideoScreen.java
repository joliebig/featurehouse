package lancs.mobilemedia.core.ui.screens;
public class CaptureVideoScreen {
  private Command takephoto=new Command("Take photo",Command.EXIT,1);
  public byte[] takePicture(){
    try {
      Alert alert=new Alert("Error","The mobile database is full",null,AlertType.INFO);
      alert.setTimeout(5000);
      display.setCurrent(alert);
      byte[] imageArray=videoControl.getSnapshot(null);
      return imageArray;
    }
 catch (    Exception e) {
      e.printStackTrace();
    }
    return null;
  }
  protected void hook46(){
    if (typescreen == CAPTUREPHOTO) {
      this.addCommand(takephoto);
    }
    original();
  }
}
