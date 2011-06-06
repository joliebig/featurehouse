package com.apple.eawt;

import java.util.EventObject;

public class ApplicationEvent extends EventObject {
	ApplicationEvent(Object o) { super(o); }
	public void setHandled(boolean b) { }
	public String getFilename() { return null; }
}
