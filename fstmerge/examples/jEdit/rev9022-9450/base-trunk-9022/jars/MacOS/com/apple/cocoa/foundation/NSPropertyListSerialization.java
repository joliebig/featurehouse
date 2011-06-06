package com.apple.cocoa.foundation;

public class NSPropertyListSerialization extends NSObject
{
	public static final int PropertyListImmutable = 0;
	
	public static native Object propertyListFromData(NSData nsd, int i, int[] ia, String[] sa);
}
