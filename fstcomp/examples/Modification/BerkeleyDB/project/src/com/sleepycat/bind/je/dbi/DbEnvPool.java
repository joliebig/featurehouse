package com.sleepycat.je.dbi; 
import java.io.File; 
import java.io.IOException; 
import java.util.Hashtable; 
import java.util.Map; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.EnvironmentConfig; 
import de.ovgu.cide.jakutil.*; 
public  class  DbEnvPool {
	 private static DbEnvPool pool=new DbEnvPool();

	 private Map envs;

	 private DbEnvPool(){ envs=new Hashtable(); }

	
public static  class  EnvironmentImplInfo {
		 public EnvironmentImpl envImpl;

		 public boolean firstHandle=false;

		 EnvironmentImplInfo( EnvironmentImpl envImpl, boolean firstHandle){ this.envImpl=envImpl; this.firstHandle=firstHandle; }


	}

	 public static DbEnvPool getInstance__wrappee__base(){ return pool; }

	 public static DbEnvPool getInstance(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getInstance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public EnvironmentImplInfo getEnvironment__wrappee__base( File envHome, EnvironmentConfig config) throws DatabaseException { return getEnvironment(envHome,config,true); }

	 public EnvironmentImplInfo getEnvironment( File envHome, EnvironmentConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getEnvironment__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public EnvironmentImplInfo getExistingEnvironment__wrappee__base( File envHome) throws DatabaseException { return getEnvironment(envHome,null,false); }

	 public EnvironmentImplInfo getExistingEnvironment( File envHome) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getExistingEnvironment__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private synchronized EnvironmentImplInfo getEnvironment__wrappee__base( File envHome, EnvironmentConfig config, boolean openIfNeeded) throws DatabaseException { boolean found; boolean firstHandle=false; EnvironmentImpl environmentImpl=null; String environmentKey=getEnvironmentMapKey(envHome); if (envs.containsKey(environmentKey)) { environmentImpl=(EnvironmentImpl)envs.get(environmentKey); if (!environmentImpl.isOpen()) { if (openIfNeeded) { environmentImpl.open(); found=true; } else { found=false; } } else { found=true; } } else { if (openIfNeeded) { environmentImpl=new EnvironmentImpl(envHome,config); envs.put(environmentKey,environmentImpl); firstHandle=true; found=true; } else { found=false; } } if (found) { return new EnvironmentImplInfo(environmentImpl,firstHandle); } else { return new EnvironmentImplInfo(null,false); } }

	 private synchronized EnvironmentImplInfo getEnvironment( File envHome, EnvironmentConfig config, boolean openIfNeeded) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getEnvironment__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void remove__wrappee__base( File envHome) throws DatabaseException { envs.remove(getEnvironmentMapKey(envHome)); }

	 void remove( File envHome) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	remove__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void clear__wrappee__base(){ envs.clear(); }

	 public void clear(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clear__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private String getEnvironmentMapKey__wrappee__base( File file) throws DatabaseException { try { return file.getCanonicalPath(); } catch ( IOException e) { throw new DatabaseException(e); } }

	 private String getEnvironmentMapKey( File file) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getEnvironmentMapKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
