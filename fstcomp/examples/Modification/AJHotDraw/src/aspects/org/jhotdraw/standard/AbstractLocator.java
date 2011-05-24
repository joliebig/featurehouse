
package org.jhotdraw.standard; 
import org.jhotdraw.framework.Handle; 
import org.jhotdraw.framework.Locator; 
public abstract  class  AbstractLocator  implements Locator/*, Storable*/, Cloneable {
		private static final long serialVersionUID = -7742023180844048409L;

		protected AbstractLocator() {	}

		public Object clone() {	try {	return super.clone();	}	catch (CloneNotSupportedException e) {	throw new InternalError();	}	}


}
