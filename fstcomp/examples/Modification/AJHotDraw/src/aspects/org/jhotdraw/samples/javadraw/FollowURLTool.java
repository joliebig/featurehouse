
package org.jhotdraw.samples.javadraw; 
import javax.swing.JApplet; 
import java.awt.event.*; 
import java.net.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.AbstractTool; 
 
class  FollowURLTool  extends AbstractTool {
	 private JApplet fApplet;

	 FollowURLTool(DrawingEditor newDrawingEditor, JApplet applet) {	super(newDrawingEditor);	fApplet = applet; }

		public void mouseMove(MouseEvent e, int x, int y) {	String urlstring = null;	Figure figure = drawing().findFigureInside(x,y);	if (figure != null) {	urlstring = (String) figure.getAttribute(FigureAttributeConstant.URL);	}	if (urlstring != null) {	fApplet.showStatus(urlstring);	}	else {	fApplet.showStatus("");	}	}

		public void mouseUp(MouseEvent e, int x, int y) {	Figure figure = getActiveDrawing().findFigureInside(x, y);	if (figure == null) {	return;	}	String urlstring = (String) figure.getAttribute(FigureAttributeConstant.URL);	if (urlstring == null) {	return;	}	try {	URL url = new URL(fApplet.getDocumentBase(), urlstring);	fApplet.getAppletContext().showDocument(url);	}	catch (MalformedURLException exception) {	fApplet.showStatus(exception.toString());	}	}


}
