package com.sleepycat.je.util; 
import java.io.File; 
import java.text.DecimalFormat; 
import com.sleepycat.je.CheckpointConfig; 
import com.sleepycat.je.Cursor; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseConfig; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.DbInternal; 
import com.sleepycat.je.Environment; 
import com.sleepycat.je.EnvironmentConfig; 
import com.sleepycat.je.EnvironmentMutableConfig; 
import com.sleepycat.je.LockMode; 
import com.sleepycat.je.OperationStatus; 
import com.sleepycat.je.Transaction; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.utilint.CmdUtil; 
import de.ovgu.cide.jakutil.*; 
public  class  DbRunAction {
	 private static final int CLEAN=1;

	 private static final int CHECKPOINT=4;

	
@MethodObject static  class  DbRunAction_main {
		 DbRunAction_main( String[] argv){ this.argv=argv; }

		 protected String[] argv;

		 protected long recoveryStart;

		 protected long actionStart;

		 protected long actionEnd;

		 protected int whichArg;

		 protected String dbName;

		 protected int doAction;

		 protected String envHome;

		 protected boolean readOnly;

		 protected String nextArg;

		 protected String action;

		 protected EnvironmentConfig envConfig;

		 protected Environment env;

		 protected CheckpointConfig forceConfig;

		 protected int nFiles;

		 protected DatabaseConfig dbConfig;

		 protected Database db;

		 protected DecimalFormat f;

		 protected long recoveryDuration;

		 protected long actionDuration;

		 void execute__wrappee__base(){ recoveryStart=0; actionStart=0; actionEnd=0; try { whichArg=0; if (argv.length == 0) { usage(); System.exit(1); } dbName=null; doAction=0; envHome="."; readOnly=false; while (whichArg < argv.length) { nextArg=argv[whichArg]; if (nextArg.equals("-h")) { whichArg++; envHome=CmdUtil.getArg(argv,whichArg); } else if (nextArg.equals("-a")) { whichArg++; action=CmdUtil.getArg(argv,whichArg); if (action.equalsIgnoreCase("clean")) { doAction=CLEAN; } else { this.hook841(); } } else if (nextArg.equals("-ro")) { readOnly=true; } else if (nextArg.equals("-s")) { dbName=argv[++whichArg]; } else { throw new IllegalArgumentException(nextArg + " is not a supported option."); } whichArg++; } envConfig=new EnvironmentConfig(); this.hook848(); this.hook847(); this.hook845(); recoveryStart=System.currentTimeMillis(); env=new Environment(new File(envHome),envConfig); forceConfig=new CheckpointConfig(); forceConfig.setForce(true); actionStart=System.currentTimeMillis(); if (doAction == CLEAN) { while (true) { nFiles=env.cleanLog(); System.out.println("Files cleaned: " + nFiles); if (nFiles == 0) { break; } } env.checkpoint(forceConfig); } this.hook840(); this.hook844(); if (doAction == CHECKPOINT) { env.checkpoint(forceConfig); } this.hook842(); this.hook838(); actionEnd=System.currentTimeMillis(); env.close(); } catch ( Exception e) { e.printStackTrace(); System.out.println(e.getMessage()); usage(); System.exit(1); } finally { f=new DecimalFormat(); f.setMaximumFractionDigits(2); recoveryDuration=actionStart - recoveryStart; System.out.println("\nrecovery time = " + f.format(recoveryDuration) + " millis "+ f.format((double)recoveryDuration / 60000)+ " minutes"); actionDuration=actionEnd - actionStart; System.out.println("action time = " + f.format(actionDuration) + " millis "+ f.format(actionDuration / 60000)+ " minutes"); } }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook838__wrappee__base() throws Exception { }

		 protected void hook838() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook838__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook839__wrappee__base() throws Exception { usage(); System.exit(1); }

		 protected void hook839() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook839__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook840__wrappee__base() throws Exception { }

		 protected void hook840() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook840__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook841__wrappee__base() throws Exception { if (action.equalsIgnoreCase("checkpoint")) { doAction=CHECKPOINT; } else { this.hook846(); } }

		 protected void hook841() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook841__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook842__wrappee__base() throws Exception { }

		 protected void hook842() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook842__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook843__wrappee__base() throws Exception { this.hook839(); }

		 protected void hook843() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook843__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook844__wrappee__base() throws Exception { }

		 protected void hook844() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook844__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook845__wrappee__base() throws Exception { }

		 protected void hook845() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook845__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook846__wrappee__base() throws Exception { this.hook843(); }

		 protected void hook846() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook846__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook847__wrappee__base() throws Exception { }

		 protected void hook847() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook847__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook848__wrappee__base() throws Exception { }

		 protected void hook848() throws Exception { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook848__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public static void main__wrappee__base( String[] argv){ new DbRunAction_main(argv).execute(); }

	 public static void main( String[] argv){ t.in(Thread.currentThread().getStackTrace()[1].toString());	main__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static String getSecs__wrappee__base( long start, long end){ return (end - start) / 1000 + " secs"; }

	 private static String getSecs( long start, long end){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSecs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static void preload__wrappee__base( Environment env, String dbName) throws DatabaseException { System.out.println("Preload starting"); Database db=env.openDatabase(null,dbName,null); Cursor cursor=db.openCursor(null,null); try { DatabaseEntry key=new DatabaseEntry(); DatabaseEntry data=new DatabaseEntry(); int count=0; while (cursor.getNext(key,data,LockMode.DEFAULT) == OperationStatus.SUCCESS) { count++; if ((count % 50000) == 0) { System.out.println(count + "..."); } } System.out.println("Preloaded " + count + " records"); } finally { cursor.close(); db.close(); } }

	 private static void preload( Environment env, String dbName) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	preload__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static void usage__wrappee__base(){ System.out.println("Usage: \n " + CmdUtil.getJavaCommand(DbRunAction.class)); System.out.println("  -h <environment home> "); System.out.println("  -a <clean|compress|evict|checkpoint|removedb>"); System.out.println("  -ro (read-only - defaults to read-write)"); System.out.println("  -s <dbName> (for preloading of evict or db remove)"); }

	 private static void usage(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	usage__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
