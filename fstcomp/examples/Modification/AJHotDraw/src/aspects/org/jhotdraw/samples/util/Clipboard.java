
package org.jhotdraw.util; 
public  class  Clipboard {
		static Clipboard fgClipboard = new Clipboard();

		static public Clipboard getClipboard() {	return fgClipboard;	}

		private Object fContents;

		private Clipboard() {	}

		public void setContents(Object contents) {	fContents = contents;	}

		public Object getContents() {	return fContents;	}


}
