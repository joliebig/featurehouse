
public class Tank {

	protected void keyPressBehandeln(int key) {
		original(key);
		int index = Math.abs(random.nextInt() % 4);
        if (fahrRichtung != tankRichtung && key > 36 && key < 41) {
            switch (index) {
                case 0:
                    SoundPlayer.getInstance().playMoveSound();
                    break;
                case 1:
                  	SoundPlayer.getInstance().playRogerSound();
                    break;
                case 2:
                    SoundPlayer.getInstance().playYessirSound();
                    break;
                case 3:
                    SoundPlayer.getInstance().playYeahSound();
                    break;
            }
        }
	}
	
	protected void explodieren() {
		original();
		SoundPlayer.getInstance().playExplodeSound();
	}
	
	protected void toolBehandeln(int toolType) {
		original(toolType);
		SoundPlayer.getInstance().playEatSound();
	}

}