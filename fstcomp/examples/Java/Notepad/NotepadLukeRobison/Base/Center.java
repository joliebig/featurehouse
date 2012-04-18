

//import the packages for using the classes in them into this class
import java.awt.*;
/**
 *A PUBLIC CLASS FOR CENTER.JAVA
 */
public class Center{
    Notepad n; //for using the object in the Notepad.java
    public Center(Notepad n){
        this.n = n;
    }
    public void nCenter(){
        //Centering the window
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        n.setLocation((screenSize.width-n.getWidth())/2,(screenSize.height-n.getHeight())/2);
    }

}
