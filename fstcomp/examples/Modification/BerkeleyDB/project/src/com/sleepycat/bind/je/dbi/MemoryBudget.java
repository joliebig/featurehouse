package com.sleepycat.je.dbi; 
import java.util.Iterator; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.DBIN; 
import com.sleepycat.je.tree.DIN; 
import com.sleepycat.je.tree.IN; 
import de.ovgu.cide.jakutil.*; 
public  class  MemoryBudget  implements EnvConfigObserver {
	
static { sinit(); }

	 public final static long MIN_MAX_MEMORY_SIZE=96 * 1024;

	 public final static String MIN_MAX_MEMORY_SIZE_STRING=Long.toString(MIN_MAX_MEMORY_SIZE);

	 private final static long N_64MB=(1 << 26);

	 private long maxMemory;

	 private long logBufferBudget;

	 private EnvironmentImpl envImpl;

	 MemoryBudget( EnvironmentImpl envImpl, DbConfigManager configManager) throws DatabaseException { this.envImpl=envImpl; envImpl.addConfigObserver(this); reset(configManager); this.hook351(configManager); }

	
@MethodObject static  class  MemoryBudget_sinit {
		 protected boolean is64;

		 protected boolean isJVM14;

		 protected String overrideArch;

		 protected String arch;

		 protected RuntimeException RE;

		 void execute__wrappee__base(){ this.hook348(); }

		 void execute(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook348__wrappee__base(){ }

		 protected void hook348(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook348__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  MemoryBudget_reset {
		 MemoryBudget_reset( MemoryBudget _this, DbConfigManager configManager){ this._this=_this; this.configManager=configManager; }

		 protected MemoryBudget _this;

		 protected DbConfigManager configManager;

		 protected long newMaxMemory;

		 protected long jvmMemory;

		 protected int maxMemoryPercent;

		 protected long newLogBufferBudget;

		 protected int numBuffers;

		 protected long startingBufferSize;

		 protected int logBufferSize;

		 protected long newCriticalThreshold;

		 protected long newTrackerBudget;

		 void execute__wrappee__base() throws DatabaseException { newMaxMemory=configManager.getLong(EnvironmentParams.MAX_MEMORY); jvmMemory=_this.getRuntimeMaxMemory(); if (newMaxMemory != 0) { if (jvmMemory < newMaxMemory) { throw new IllegalArgumentException(EnvironmentParams.MAX_MEMORY.getName() + " has a value of " + newMaxMemory+ " but the JVM is only configured for "+ jvmMemory+ ". Consider using je.maxMemoryPercent."); } if (newMaxMemory < _this.MIN_MAX_MEMORY_SIZE) { throw new IllegalArgumentException(EnvironmentParams.MAX_MEMORY.getName() + " is " + newMaxMemory+ " which is less than the minimum: "+ _this.MIN_MAX_MEMORY_SIZE); } } else { if (jvmMemory == Long.MAX_VALUE) { jvmMemory=_this.N_64MB; } maxMemoryPercent=configManager.getInt(EnvironmentParams.MAX_MEMORY_PERCENT); newMaxMemory=(maxMemoryPercent * jvmMemory) / 100; } newLogBufferBudget=configManager.getLong(EnvironmentParams.LOG_MEM_SIZE); if (newLogBufferBudget == 0) { newLogBufferBudget=newMaxMemory >> 4; } else if (newLogBufferBudget > newMaxMemory / 2) { newLogBufferBudget=newMaxMemory / 2; } numBuffers=configManager.getInt(EnvironmentParams.NUM_LOG_BUFFERS); startingBufferSize=newLogBufferBudget / numBuffers; logBufferSize=configManager.getInt(EnvironmentParams.LOG_BUFFER_MAX_SIZE); if (startingBufferSize > logBufferSize) { startingBufferSize=logBufferSize; newLogBufferBudget=numBuffers * startingBufferSize; } else if (startingBufferSize < EnvironmentParams.MIN_LOG_BUFFER_SIZE) { startingBufferSize=EnvironmentParams.MIN_LOG_BUFFER_SIZE; newLogBufferBudget=numBuffers * startingBufferSize; } this.hook350(); newTrackerBudget=(newMaxMemory * _this.envImpl.getConfigManager().getInt(EnvironmentParams.CLEANER_DETAIL_MAX_MEMORY_PERCENTAGE)) / 100; _this.maxMemory=newMaxMemory; this.hook349(); _this.logBufferBudget=newLogBufferBudget; }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook349__wrappee__base() throws DatabaseException { }

		 protected void hook349() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook349__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook350__wrappee__base() throws DatabaseException { }

		 protected void hook350() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook350__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 private static void sinit__wrappee__base(){ new MemoryBudget_sinit().execute(); }

	 private static void sinit(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sinit__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void envConfigUpdate__wrappee__base( DbConfigManager configManager) throws DatabaseException { long oldLogBufferBudget=logBufferBudget; reset(configManager); if (oldLogBufferBudget != logBufferBudget) { envImpl.getLogManager().resetPool(configManager); } }

	 public void envConfigUpdate( DbConfigManager configManager) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	envConfigUpdate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void reset__wrappee__base( DbConfigManager configManager) throws DatabaseException { new MemoryBudget_reset(this,configManager).execute(); }

	 private void reset( DbConfigManager configManager) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	reset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long getRuntimeMaxMemory__wrappee__base(){ if ("Mac OS X".equals(System.getProperty("os.name"))) { String jvmVersion=System.getProperty("java.version"); if (jvmVersion != null && jvmVersion.startsWith("1.4.2")) { return Long.MAX_VALUE; } } return Runtime.getRuntime().maxMemory(); }

	 public static long getRuntimeMaxMemory(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getRuntimeMaxMemory__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLogBufferBudget__wrappee__base(){ return logBufferBudget; }

	 public long getLogBufferBudget(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogBufferBudget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getMaxMemory__wrappee__base(){ return maxMemory; }

	 public long getMaxMemory(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMaxMemory__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook351__wrappee__base( DbConfigManager configManager) throws DatabaseException { }

	 protected void hook351( DbConfigManager configManager) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook351__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
