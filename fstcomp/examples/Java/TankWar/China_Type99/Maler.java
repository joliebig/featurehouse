
public class Maler {

	protected void tankErstellen(){
		original();
		int x, y;
		x = GAME_WIDTH * 2 / 3 / 3;
		y = (int) (2.5 * x);
		menu.add(Sprach.TANKA, loadImage("choice11.png",x,y), loadImage("choice01.png",x,y), 0);
	}
}