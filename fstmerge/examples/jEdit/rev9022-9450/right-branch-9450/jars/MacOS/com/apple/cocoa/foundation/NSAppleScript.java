package com.apple.cocoa.foundation;

public class NSAppleScript extends NSObject
{
	public NSAppleScript(String s) { }
	public native boolean compile(NSMutableDictionary nsmd);
	public native NSAppleEventDescriptor execute(NSMutableDictionary nsmd);
}
