
package org.jhotdraw.util; 
import java.util.EventObject; 
public  interface  CommandListener {
		public void commandExecuted(EventObject commandEvent);

		public void commandExecutable(EventObject commandEvent);

		public void commandNotExecutable(EventObject commandEvent);


}
