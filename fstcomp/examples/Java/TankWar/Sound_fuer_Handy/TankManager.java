
public class TankManager {

	protected boolean sound = false;

	public void aktualisieren() {
		original();
		if (status != GameManager.SPIELEN) {
			if (!sound) {
				SoundPlayer.getInstance().playBgSound();
				sound = true;
			}
		} else if (sound == true) {
			SoundPlayer.getInstance().stopSound();
			sound = false;
		}
	}



}