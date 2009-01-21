
package org.jhotdraw.util; 
import java.io.*; 
public  interface  Storable {
		public void write(StorableOutput dw);

		public void read(StorableInput dr) throws IOException;


}
