

//import the packages for using the classes in them into this class
import java.awt.*;
/**
 *A PUBLIC CLASS FOR CENTER.JAVA
 */
public class fCenter{
	Fonts f; //for using the object in the Fonts.java
	public fCenter(Fonts f){
		this.f = f;
	}
	public void fCenter(){
		//Centering the window
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		f.setLocation((screenSize.width-f.getWidth())/2,(screenSize.height-f.getHeight())/2);
	}
}
