package com.sleepycat.je.utilint; 
import de.ovgu.cide.jakutil.*; 
public  class  HexFormatter {
	 static public String formatLong__wrappee__base( long l){ StringBuffer sb=new StringBuffer(); sb.append(Long.toHexString(l)); sb.insert(0,"0000000000000000".substring(0,16 - sb.length())); sb.insert(0,"0x"); return sb.toString(); }

	 static public String formatLong( long l){ t.in(Thread.currentThread().getStackTrace()[1].toString());	formatLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
