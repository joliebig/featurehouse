package com.sleepycat.je.util; 
import java.io.File; 
import java.io.PrintStream; 
import java.util.Arrays; 
import java.util.Iterator; 
import java.util.Map; 
import java.util.SortedMap; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.DbInternal; 
import com.sleepycat.je.Environment; 
import com.sleepycat.je.EnvironmentConfig; 
import com.sleepycat.je.JEVersion; 
import com.sleepycat.je.cleaner.FileSummary; 
import com.sleepycat.je.cleaner.UtilizationProfile; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.utilint.CmdUtil; 
import de.ovgu.cide.jakutil.*; 
public  class  DbSpace {
	 private static final String USAGE="usage: " + CmdUtil.getJavaCommand(DbSpace.class) + "\n"+ "       -h <dir> # environment home directory\n"+ "       [-q]     # quiet, print grand totals only\n"+ "       [-u]     # sort by utilization\n"+ "       [-d]     # dump file summary details\n"+ "       [-V]     # print JE version number";

	 private File envHome=null;

	 private EnvironmentImpl envImpl;

	 private boolean quiet=false;

	 private boolean sorted=false;

	 private boolean details=false;

	 private DbSpace(){ }

	 public DbSpace( Environment env, boolean quiet, boolean details, boolean sorted){ this(DbInternal.envGetEnvironmentImpl(env),quiet,details,sorted); }

	 public DbSpace( EnvironmentImpl envImpl, boolean quiet, boolean details, boolean sorted){ this.envImpl=envImpl; this.quiet=quiet; this.details=details; this.sorted=sorted; }

	
private static  class  Summary  implements Comparable {
		 static final String HEADER="  File    Size (KB)  % Used\n" + "--------  ---------  ------";

		 Long fileNum;

		 long totalSize;

		 long obsoleteSize;

		 Summary(){ }

		 Summary( Long fileNum, FileSummary summary) throws DatabaseException { this.fileNum=fileNum; totalSize=summary.totalSize; obsoleteSize=summary.getObsoleteSize(); }

		 public int compareTo__wrappee__base( Object other){ Summary o=(Summary)other; return utilization() - o.utilization(); }

		 public int compareTo( Object other){ t.in(Thread.currentThread().getStackTrace()[1].toString());	compareTo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 void add__wrappee__base( Summary o){ totalSize+=o.totalSize; obsoleteSize+=o.obsoleteSize; }

		 void add( Summary o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	add__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 void print__wrappee__base( PrintStream out){ if (fileNum != null) { pad(out,Long.toHexString(fileNum.longValue()),8,'0'); } else { out.print(" TOTALS "); } int kb=(int)(totalSize / 1024); int util=utilization(); out.print("  "); pad(out,Integer.toString(kb),9,' '); out.print("     "); pad(out,Integer.toString(util),3,' '); out.println(); }

		 void print( PrintStream out){ t.in(Thread.currentThread().getStackTrace()[1].toString());	print__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 int utilization__wrappee__base(){ return UtilizationProfile.utilization(obsoleteSize,totalSize); }

		 int utilization(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	utilization__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 private void pad__wrappee__base( PrintStream out, String val, int digits, char padChar){ int padSize=digits - val.length(); for (int i=0; i < padSize; i+=1) { out.print(padChar); } out.print(val); }

		 private void pad( PrintStream out, String val, int digits, char padChar){ t.in(Thread.currentThread().getStackTrace()[1].toString());	pad__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public static void main__wrappee__base( String argv[]) throws DatabaseException { DbSpace space=new DbSpace(); space.parseArgs(argv); EnvironmentConfig envConfig=new EnvironmentConfig(); envConfig.setReadOnly(true); Environment env=new Environment(space.envHome,envConfig); space.envImpl=DbInternal.envGetEnvironmentImpl(env); try { space.print(System.out); System.exit(0); } catch ( Throwable e) { e.printStackTrace(System.err); System.exit(1); } finally { try { env.close(); } catch ( Throwable e) { e.printStackTrace(System.err); System.exit(1); } } }

	 public static void main( String argv[]) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	main__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void printUsage__wrappee__base( String msg){ if (msg != null) { System.err.println(msg); } System.err.println(USAGE); System.exit(-1); }

	 private void printUsage( String msg){ t.in(Thread.currentThread().getStackTrace()[1].toString());	printUsage__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void parseArgs__wrappee__base( String argv[]){ int argc=0; int nArgs=argv.length; if (nArgs == 0) { printUsage(null); System.exit(0); } while (argc < nArgs) { String thisArg=argv[argc++]; if (thisArg.equals("-q")) { quiet=true; } else if (thisArg.equals("-u")) { sorted=true; } else if (thisArg.equals("-d")) { details=true; } else if (thisArg.equals("-V")) { System.out.println(JEVersion.CURRENT_VERSION); System.exit(0); } else if (thisArg.equals("-h")) { if (argc < nArgs) { envHome=new File(argv[argc++]); } else { printUsage("-h requires an argument"); } } } if (envHome == null) { printUsage("-h is a required argument"); } }

	 private void parseArgs( String argv[]){ t.in(Thread.currentThread().getStackTrace()[1].toString());	parseArgs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void print__wrappee__base( PrintStream out) throws DatabaseException { UtilizationProfile profile=envImpl.getUtilizationProfile(); SortedMap map=profile.getFileSummaryMap(false); int fileIndex=0; Summary totals=new Summary(); Summary[] summaries=null; if (!quiet) { summaries=new Summary[map.size()]; } Iterator iter=map.entrySet().iterator(); while (iter.hasNext()) { Map.Entry entry=(Map.Entry)iter.next(); Long fileNum=(Long)entry.getKey(); FileSummary fs=(FileSummary)entry.getValue(); Summary summary=new Summary(fileNum,fs); if (summaries != null) { summaries[fileIndex]=summary; } if (details) { out.println("File 0x" + Long.toHexString(fileNum.longValue()) + ": "+ fs); } totals.add(summary); fileIndex+=1; } if (details) { out.println(); } out.println(Summary.HEADER); if (summaries != null) { if (sorted) { Arrays.sort(summaries); } for (int i=0; i < summaries.length; i+=1) { summaries[i].print(out); } } totals.print(out); }

	 public void print( PrintStream out) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	print__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
