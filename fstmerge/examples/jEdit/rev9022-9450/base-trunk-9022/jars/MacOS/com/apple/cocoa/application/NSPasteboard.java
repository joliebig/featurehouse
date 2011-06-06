package com.apple.cocoa.application;

import com.apple.cocoa.foundation.NSData;
import com.apple.cocoa.foundation.NSObject;

public class NSPasteboard extends NSObject
{
	public native NSData dataForType(String s);
	public native String stringForType(String s);
}
