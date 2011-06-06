package com.apple.cocoa.application;

import com.apple.cocoa.foundation.NSCoding;
import com.apple.cocoa.foundation.NSObject;
import com.apple.cocoa.foundation.NSSelector;

public class NSMenuItem extends NSObject implements _NSObsoleteMenuItemProtocol, NSCoding, NSValidatedUserInterfaceItem
{
	public NSMenuItem() { }
	public NSMenuItem(String s1, NSSelector nss, String s2) { }
	public native void setSubmenu(NSMenu nsm);
	public native void setTarget(Object o);
	public NSMenuItem separatorItem() { return null; }
	public native void setEnabled(boolean b);
}
