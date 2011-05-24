
package org.jhotdraw.framework; 
import java.util.EventObject; 
public  interface  ToolListener {
		public void toolEnabled(EventObject toolEvent);

		public void toolDisabled(EventObject toolEvent);

		public void toolUsable(EventObject toolEvent);

		public void toolUnusable(EventObject toolEvent);

		public void toolActivated(EventObject toolEvent);

		public void toolDeactivated(EventObject toolEvent);


}
