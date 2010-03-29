
import java.io.IOException;

public class TankManager {
protected boolean sound = false;

	public void aktualisieren() {
		original();
		if (status != GameManager.SPIELEN) {
			if (!sound) {
				try {
					SoundPlayer.getInstance().playBgSound();
					sound = true;
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		} else if (sound == true) {
			SoundPlayer.getInstance().stopSound();
			sound = false;
		}
	}

}