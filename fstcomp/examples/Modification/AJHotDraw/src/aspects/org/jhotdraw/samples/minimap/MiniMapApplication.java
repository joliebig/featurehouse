
package org.jhotdraw.samples.minimap; 
import org.jhotdraw.contrib.Desktop; 
import org.jhotdraw.contrib.SplitPaneDrawApplication; 
public  class  MiniMapApplication  extends SplitPaneDrawApplication {
		protected Desktop createDesktop() {	return new MiniMapDesktop();	}

		public static void main(String[] args) {	MiniMapApplication window = new MiniMapApplication();	window.open();	}


}
