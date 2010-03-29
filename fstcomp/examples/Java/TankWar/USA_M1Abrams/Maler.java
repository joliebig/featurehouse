
public class Maler {
	
	protected void tankErstellen(){
		original();
		int x, y;
		x = GAME_WIDTH * 2 / 3 / 3;
		y = (int) (2.5 * x);
		menu.add(Sprach.TANKC, loadImage("choice33.png",x,y),loadImage("choice03.png",x,y), 3);
	}
}