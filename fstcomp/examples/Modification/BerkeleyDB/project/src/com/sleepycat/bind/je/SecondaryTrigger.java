package com.sleepycat.je; 
import com.sleepycat.je.txn.Locker; 
import de.ovgu.cide.jakutil.*; 
 
class  SecondaryTrigger  implements DatabaseTrigger {
	 private SecondaryDatabase secDb;

	 SecondaryTrigger( SecondaryDatabase secDb){ this.secDb=secDb; }

	 final SecondaryDatabase getDb__wrappee__base(){ return secDb; }

	 final SecondaryDatabase getDb(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDb__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void triggerAdded__wrappee__base( Database db){ }

	 public void triggerAdded( Database db){ t.in(Thread.currentThread().getStackTrace()[1].toString());	triggerAdded__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void triggerRemoved__wrappee__base( Database db){ secDb.clearPrimary(); }

	 public void triggerRemoved( Database db){ t.in(Thread.currentThread().getStackTrace()[1].toString());	triggerRemoved__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void databaseUpdated__wrappee__base( Database db, Locker locker, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { secDb.updateSecondary(locker,null,priKey,oldData,newData); }

	 public void databaseUpdated( Database db, Locker locker, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	databaseUpdated__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
