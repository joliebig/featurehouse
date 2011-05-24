package com.sleepycat.je.dbi; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.EnvironmentConfig; 
import com.sleepycat.je.config.BooleanConfigParam; 
import com.sleepycat.je.config.ConfigParam; 
import com.sleepycat.je.config.IntConfigParam; 
import com.sleepycat.je.config.LongConfigParam; 
import com.sleepycat.je.config.ShortConfigParam; 
import de.ovgu.cide.jakutil.*; 
public  class  DbConfigManager {
	 private EnvironmentConfig environmentConfig;

	 public DbConfigManager( EnvironmentConfig config) throws DbConfigException { environmentConfig=config; }

	 public EnvironmentConfig getEnvironmentConfig__wrappee__base(){ return environmentConfig; }

	 public EnvironmentConfig getEnvironmentConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEnvironmentConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized String get__wrappee__base( ConfigParam configParam) throws IllegalArgumentException { return environmentConfig.getConfigParam(configParam.getName()); }

	 public synchronized String get( ConfigParam configParam) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	get__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized String get__wrappee__base( String configParamName) throws IllegalArgumentException { return environmentConfig.getConfigParam(configParamName); }

	 public synchronized String get( String configParamName) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	get__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getBoolean__wrappee__base( BooleanConfigParam configParam) throws DatabaseException { String val=get(configParam); return Boolean.valueOf(val).booleanValue(); }

	 public boolean getBoolean( BooleanConfigParam configParam) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getBoolean__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public short getShort__wrappee__base( ShortConfigParam configParam) throws DatabaseException { String val=get(configParam); short shortValue=0; try { shortValue=Short.parseShort(val); } catch ( NumberFormatException e) { assert false : e.getMessage(); } return shortValue; }

	 public short getShort( ShortConfigParam configParam) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getShort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getInt__wrappee__base( IntConfigParam configParam) throws DatabaseException { String val=get(configParam); int intValue=0; if (val != null) { try { intValue=Integer.parseInt(val); } catch ( NumberFormatException e) { assert false : e.getMessage(); } } return intValue; }

	 public int getInt( IntConfigParam configParam) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLong__wrappee__base( LongConfigParam configParam) throws DatabaseException { String val=get(configParam); long longValue=0; if (val != null) { try { longValue=Long.parseLong(val); } catch ( NumberFormatException e) { assert false : e.getMessage(); } } return longValue; }

	 public long getLong( LongConfigParam configParam) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
