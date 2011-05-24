
package org.jhotdraw.util; 
import org.jhotdraw.framework.DrawingEditor; 
public  interface  Command {
		public void execute();

		public boolean isExecutable();

		public String name();

		public DrawingEditor getDrawingEditor();


}
