
public class Tank {

	protected void explodieren(){
		original();
		tankManager.expEffekt.addElement(new ExplodierenEffekt(tankManager, x_Koordinate,
				y_Koordinate));
	}

}