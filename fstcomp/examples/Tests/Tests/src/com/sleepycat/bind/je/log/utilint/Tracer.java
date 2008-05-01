package com.sleepycat.je.utilint; 
import java.io.PrintWriter; 
import java.io.StringWriter; 
import java.nio.ByteBuffer; 
import java.sql.Timestamp; 
import java.util.Calendar; 
import java.util.logging.Level; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.config.ConfigParam; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import de.ovgu.cide.jakutil.*; 
public  class  Tracer {
	 public static final String INFO_FILES="je.info";

	 private Timestamp time;

	 private String msg;

	 public Tracer( String msg){ this.time=getCurrentTimestamp(); this.msg=msg; }

	 public static void trace( Level logLevel, EnvironmentImpl envImpl, String msg){ }

	 public static Level parseLevel( EnvironmentImpl envImpl, ConfigParam configParam) throws DatabaseException { Level level=null; try { String levelVal=envImpl.getConfigManager().get(configParam); level=Level.parse(levelVal); } catch ( IllegalArgumentException e) { throw new DatabaseException("Problem parsing parameter " + configParam.getName() + ": "+ e.getMessage(),e); } return level; }

	 private Timestamp getCurrentTimestamp(){ Calendar cal=Calendar.getInstance(); return new Timestamp(cal.getTime().getTime()); }

	 public static String getStackTrace( Throwable t){ StringWriter s=new StringWriter(); t.printStackTrace(new PrintWriter(s)); String stackTrace=s.toString(); stackTrace=stackTrace.replaceAll("<","&lt;"); stackTrace=stackTrace.replaceAll(">","&gt;"); return stackTrace; }


}
