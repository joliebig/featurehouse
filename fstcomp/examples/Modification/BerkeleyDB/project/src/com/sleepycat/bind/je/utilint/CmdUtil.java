package com.sleepycat.je.utilint; 
import java.io.File; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.EnvironmentConfig; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  CmdUtil {
	 private static final String printableChars="!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";

	 public static String getArg__wrappee__base( String[] argv, int whichArg) throws IllegalArgumentException { if (whichArg < argv.length) { return argv[whichArg]; } else { throw new IllegalArgumentException(); } }

	 public static String getArg( String[] argv, int whichArg) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getArg__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long readLongNumber__wrappee__base( String longVal){ if (longVal.startsWith("0x")) { return Long.parseLong(longVal.substring(2),16); } else { return Long.parseLong(longVal); } }

	 public static long readLongNumber( String longVal){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readLongNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void formatEntry__wrappee__base( StringBuffer sb, byte[] entryData, boolean formatUsingPrintable){ for (int i=0; i < entryData.length; i++) { int b=entryData[i] & 0xff; if (formatUsingPrintable) { if (isPrint(b)) { if (b == 0134) { sb.append('\\'); } sb.append(printableChars.charAt(b - 33)); } else { sb.append('\\'); String hex=Integer.toHexString(b); if (b < 16) { sb.append('0'); } sb.append(hex); } } else { String hex=Integer.toHexString(b); if (b < 16) { sb.append('0'); } sb.append(hex); } } }

	 public static void formatEntry( StringBuffer sb, byte[] entryData, boolean formatUsingPrintable){ t.in(Thread.currentThread().getStackTrace()[1].toString());	formatEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static boolean isPrint__wrappee__base( int b){ return (b < 0177) && (040 < b); }

	 private static boolean isPrint( int b){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isPrint__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static EnvironmentImpl makeUtilityEnvironment__wrappee__base( File envHome, boolean readOnly) throws DatabaseException { EnvironmentConfig config=new EnvironmentConfig(); config.setReadOnly(readOnly); hook853(config); hook854(config); hook855(config); config.setConfigParam(EnvironmentParams.ENV_RECOVERY.getName(),"false"); EnvironmentImpl envImpl=new EnvironmentImpl(envHome,config); return envImpl; }

	 public static EnvironmentImpl makeUtilityEnvironment( File envHome, boolean readOnly) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	makeUtilityEnvironment__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String getJavaCommand__wrappee__base( Class cls){ String clsName=cls.getName(); String lastName=clsName.substring(clsName.lastIndexOf('.') + 1); return "java { " + cls.getName() + " | -jar je.jar "+ lastName+ " }"; }

	 public static String getJavaCommand( Class cls){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getJavaCommand__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected static void hook853__wrappee__base( EnvironmentConfig config) throws DatabaseException { }

	 protected static void hook853( EnvironmentConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook853__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected static void hook854__wrappee__base( EnvironmentConfig config) throws DatabaseException { }

	 protected static void hook854( EnvironmentConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook854__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected static void hook855__wrappee__base( EnvironmentConfig config) throws DatabaseException { }

	 protected static void hook855( EnvironmentConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook855__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
