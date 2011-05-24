package com.sleepycat.je.tree; 
import de.ovgu.cide.jakutil.*; 
public  class  TreeUtils {
	 static private final String SPACES="                                " + "                                " + "                                "+ "                                ";

	 public static String indent__wrappee__base( int nSpaces){ return SPACES.substring(0,nSpaces); }

	 public static String indent( int nSpaces){ t.in(Thread.currentThread().getStackTrace()[1].toString());	indent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String dumpByteArray__wrappee__base( byte[] b){ StringBuffer sb=new StringBuffer(); if (b != null) { if (Key.DUMP_BINARY) { for (int i=0; i < b.length; i++) { sb.append(b[i] & 0xFF); sb.append(" "); } } else { sb.append(new String(b)); } } else { sb.append("null"); } return sb.toString(); }

	 public static String dumpByteArray( byte[] b){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpByteArray__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
