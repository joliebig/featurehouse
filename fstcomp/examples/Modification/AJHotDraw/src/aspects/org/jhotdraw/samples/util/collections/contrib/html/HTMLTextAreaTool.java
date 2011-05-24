
package org.jhotdraw.contrib.html; 
import java.awt.Font; 
import org.jhotdraw.contrib.TextAreaTool; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.standard.TextHolder; 
public  class  HTMLTextAreaTool  extends TextAreaTool {
		public HTMLTextAreaTool(DrawingEditor newDrawingEditor, Figure prototype) {	super(newDrawingEditor, prototype);	}

		protected Font getFont(TextHolder figure) {	return new Font("Helvetica", Font.PLAIN, 12);	}


}
