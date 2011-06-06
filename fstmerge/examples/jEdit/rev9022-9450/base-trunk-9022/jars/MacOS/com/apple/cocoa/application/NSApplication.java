package com.apple.cocoa.application;

public class NSApplication extends NSResponder
{
	public static native NSApplication sharedApplication();
	public native void setServicesProvider(Object o);
	public native void setDelegate(Object o);
}
