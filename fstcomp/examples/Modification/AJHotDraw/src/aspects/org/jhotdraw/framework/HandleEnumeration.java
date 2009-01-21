
package org.jhotdraw.framework; 
import java.util.List; 
public  interface  HandleEnumeration {
		public Handle nextHandle();

		public boolean hasNextHandle();

		public List toList();

		public void reset();


}
