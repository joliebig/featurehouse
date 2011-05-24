
package org.jhotdraw.framework; 
public  class  JHotDrawRuntimeException  extends RuntimeException {
		private Exception myNestedException;

		public JHotDrawRuntimeException(String msg) {	super(msg);	}

		public JHotDrawRuntimeException(Exception nestedException) {	this(nestedException.getLocalizedMessage());	setNestedException(nestedException);	nestedException.fillInStackTrace();	}

		protected void setNestedException(Exception newNestedException) {	myNestedException = newNestedException;	}

		public Exception getNestedException() {	return myNestedException;	}


}
