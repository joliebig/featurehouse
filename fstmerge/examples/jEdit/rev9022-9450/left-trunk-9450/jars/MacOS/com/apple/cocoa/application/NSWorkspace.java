package com.apple.cocoa.application;

import com.apple.cocoa.foundation.NSObject;

public class NSWorkspace extends NSObject
{
	public static native NSWorkspace sharedWorkspace();
	public native boolean selectFile(String s1, String s2);
}
