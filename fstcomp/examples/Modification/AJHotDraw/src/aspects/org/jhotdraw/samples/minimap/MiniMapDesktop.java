
package org.jhotdraw.samples.minimap; 
import org.jhotdraw.contrib.SplitPaneDesktop; 
import org.jhotdraw.contrib.MiniMapView; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.util.Iconkit; 
import org.jhotdraw.figures.ImageFigure; 
import javax.swing.*; 
import java.awt.*; 
public  class  MiniMapDesktop  extends SplitPaneDesktop {
		private String imageName = "/CH/ifa/draw/samples/javadraw/sampleimages/view.gif";

		protected Component createRightComponent(DrawingView view) {	Image image = Iconkit.instance().registerAndLoadImage(	(Component)view, imageName);	view.add(new ImageFigure(image, imageName, new Point(0,0)));	view.checkDamage();	return super.createRightComponent(view);	}

		protected Component createLeftComponent(DrawingView view) {	JPanel blankPanel = new JPanel();	MiniMapView mmv = new MiniMapView(view, (JScrollPane)getRightComponent());	JSplitPane leftSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, blankPanel, mmv);	leftSplitPane.setOneTouchExpandable(true);	leftSplitPane.setDividerLocation(200);	return leftSplitPane;	}


}
