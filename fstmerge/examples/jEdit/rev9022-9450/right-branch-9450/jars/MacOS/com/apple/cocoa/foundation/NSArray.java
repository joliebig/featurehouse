package com.apple.cocoa.foundation;

public class NSArray extends NSObject implements NSCoding
{
	public native int count();
	public native Object objectAtIndex(int i);
}
