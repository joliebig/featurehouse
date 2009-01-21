
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.DrawingView; 
public  interface  Desktop {
		public final static int PRIMARY = 0;

		public final static int SECONDARY = 1;

		public final static int TERTIARY = 2;

		public DrawingView getActiveDrawingView();

		public void addToDesktop(DrawingView dv, int location);

		public void removeFromDesktop(DrawingView dv, int location);

		public void removeAllFromDesktop(int location);

		public DrawingView[] getAllFromDesktop(int location);

		public void updateTitle(String newDrawingTitle);

		public void addDesktopListener(DesktopListener dpl);

		public void removeDesktopListener(DesktopListener dpl);


}
