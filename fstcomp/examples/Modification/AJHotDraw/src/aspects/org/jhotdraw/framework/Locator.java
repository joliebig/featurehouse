
package org.jhotdraw.framework; 
import java.awt.Point; 
import java.io.Serializable; 
import org.jhotdraw.framework.Figure; 
public  interface  Locator  extends Serializable, Cloneable {
		public Point locate(Figure owner);


}
