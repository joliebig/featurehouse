
package org.jhotdraw.framework; 
public  class  JHotDrawException  extends Exception {
		private Exception myNestedException;

		public JHotDrawException(String msg) {	super(msg);	}

		public JHotDrawException(Exception nestedException) {	this(nestedException.getLocalizedMessage());	setNestedException(nestedException);	nestedException.fillInStackTrace();	}

		protected void setNestedException(Exception newNestedException) {	myNestedException = newNestedException;	}

		public Exception getNestedException() {	return myNestedException;	}


}
