
package org.jhotdraw.contrib; 
import java.awt.Insets; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.io.Serializable; 
public  interface  Layouter  extends Serializable {
		public Rectangle calculateLayout(Point origin, Point corner);

		public Rectangle layout(Point origin, Point corner);

		public void setInsets(Insets newInsets);

		public Insets getInsets();

		public Layouter create(Layoutable newLayoutable);


}
