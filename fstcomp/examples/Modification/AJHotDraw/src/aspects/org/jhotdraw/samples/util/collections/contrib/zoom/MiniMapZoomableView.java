
package org.jhotdraw.contrib.zoom; 
import org.jhotdraw.contrib.MiniMapView; 
import org.jhotdraw.framework.DrawingView; 
import java.awt.geom.AffineTransform; 
import java.awt.geom.NoninvertibleTransformException; 
import javax.swing.JScrollPane; 
public  class  MiniMapZoomableView  extends MiniMapView {
		public MiniMapZoomableView(DrawingView newMappedDrawingView, JScrollPane subject) {	super(newMappedDrawingView, subject);	}

		public AffineTransform getInverseSubjectTransform() {	double subjectsScale = ((ZoomDrawingView)getMappedComponent()).getScale();	AffineTransform at = null;	try {	at = AffineTransform.getScaleInstance(subjectsScale, subjectsScale).createInverse();	}	catch (NoninvertibleTransformException nte) {	}	return at;	}


}
