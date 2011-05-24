
import javax.microedition.midlet.MIDlet;
import javax.microedition.midlet.MIDletStateChangeException;

public class TankManager {

	protected void destroyApp(boolean arg0){
		
	}

	protected void pauseApp() {
		
	}

	protected void startApp() throws MIDletStateChangeException {
			
	}
	
	public void exit(){
		destroyApp(true);
        notifyDestroyed();
	}

}