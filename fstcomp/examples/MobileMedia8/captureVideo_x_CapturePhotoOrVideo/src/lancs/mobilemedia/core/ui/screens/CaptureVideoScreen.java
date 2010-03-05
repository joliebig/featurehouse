package lancs.mobilemedia.core.ui.screens;
public class CaptureVideoScreen {
  private Command start=new Command("Start",Command.EXIT,1);
  private Command stop=new Command("Stop",Command.ITEM,1);
  public void startCapture(){
    try {
      if (!recording) {
        rControl=(RecordControl)capturePlayer.getControl("RecordControl");
        if (rControl == null)         throw new Exception("No RecordControl found!");
        byteOfArray=new ByteArrayOutputStream();
        rControl.setRecordStream(byteOfArray);
        rControl.startRecord();
        recording=true;
      }
    }
 catch (    Exception e) {
      e.printStackTrace();
    }
  }
  public void pauseCapture(){
    try {
      if (recording) {
        rControl.stopRecord();
        rControl.commit();
        recording=false;
      }
    }
 catch (    Exception e) {
      e.printStackTrace();
    }
  }
  public byte[] getByteArrays(){
    return byteOfArray.toByteArray();
  }
  protected void hook47(){
    if (typescreen == CAPTUREVIDEO) {
      this.addCommand(start);
      this.addCommand(stop);
    }
    original();
  }
}
