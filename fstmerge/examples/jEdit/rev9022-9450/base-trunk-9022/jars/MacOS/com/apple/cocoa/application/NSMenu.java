package com.apple.cocoa.application;

import com.apple.cocoa.foundation.NSCoding;
import com.apple.cocoa.foundation.NSObject;

public class NSMenu extends NSObject implements NSCoding
{
	public void addItem(NSMenuItem nsmi) { }
	public native int numberOfItems();
	public native void removeItemAtIndex(int i);
}
