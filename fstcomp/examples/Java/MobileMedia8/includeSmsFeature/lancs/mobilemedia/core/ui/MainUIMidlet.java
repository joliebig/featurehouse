package lancs.mobilemedia.core.ui;
import lancs.mobilemedia.sms.SmsReceiverController;
import lancs.mobilemedia.sms.SmsReceiverThread;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook10() throws MIDletStateChangeException {
      controller=new SmsReceiverController(_this,_this.imageModel,album);
      controller.setNextController(albumController);
      smsR=new SmsReceiverThread(_this,_this.imageModel,album,controller);
      System.out.println("SmsController::Starting SMSReceiver Thread");
      new Thread(smsR).start();
      original();
    }
  }
}
