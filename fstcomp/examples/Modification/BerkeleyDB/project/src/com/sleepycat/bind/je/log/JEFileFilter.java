package com.sleepycat.je.log; 
import java.io.File; 
import java.io.FilenameFilter; 
import java.util.StringTokenizer; 
import de.ovgu.cide.jakutil.*; 
 
class  JEFileFilter  implements FilenameFilter {
	 String[] suffix;

	 JEFileFilter( String[] suffix){ this.suffix=suffix; }

	 private boolean matches__wrappee__base( String fileSuffix){ for (int i=0; i < suffix.length; i++) { if (fileSuffix.equalsIgnoreCase(suffix[i])) { return true; } } return false; }

	 private boolean matches( String fileSuffix){ t.in(Thread.currentThread().getStackTrace()[1].toString());	matches__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean accept__wrappee__base( File dir, String name){ boolean ok=false; StringTokenizer tokenizer=new StringTokenizer(name,"."); int nTokens=tokenizer.countTokens(); if (nTokens == 2 || nTokens == 3) { boolean hasVersion=(nTokens == 3); String fileNumber=tokenizer.nextToken(); String fileSuffix="." + tokenizer.nextToken(); String fileVersion=(hasVersion ? tokenizer.nextToken() : null); if ((fileNumber.length() == 8) && matches(fileSuffix)) { try { Integer.parseInt(fileNumber,16); ok=true; } catch ( NumberFormatException e) { ok=false; } if (hasVersion) { try { Integer.parseInt(fileVersion); ok=true; } catch ( NumberFormatException e) { ok=false; } } } } return ok; }

	 public boolean accept( File dir, String name){ t.in(Thread.currentThread().getStackTrace()[1].toString());	accept__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
