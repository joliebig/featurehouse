
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.Figure; 
public  interface  Layoutable  extends Figure {
		public void layout();

		public void setLayouter(Layouter newLayouter);

		public Layouter getLayouter();


}
