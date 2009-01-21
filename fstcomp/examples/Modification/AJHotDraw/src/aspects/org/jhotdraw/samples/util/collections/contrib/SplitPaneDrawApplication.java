
package org.jhotdraw.contrib; 
import org.jhotdraw.application.*; 
public  class  SplitPaneDrawApplication  extends DrawApplication {
		public SplitPaneDrawApplication() {	this("AJHotDraw");	}

		public SplitPaneDrawApplication(String title) {	super(title);	}

		protected Desktop createDesktop() {	return new SplitPaneDesktop();	}


}
