package com.sleepycat.je; 
import com.sleepycat.je.txn.Locker; 
import de.ovgu.cide.jakutil.*; 
 
class  ForeignKeyTrigger  implements DatabaseTrigger {
	 private SecondaryDatabase secDb;

	 ForeignKeyTrigger( SecondaryDatabase secDb){ this.secDb=secDb; }

	 public void triggerAdded__wrappee__base( Database db){ }

	 public void triggerAdded( Database db){ t.in(Thread.currentThread().getStackTrace()[1].toString());	triggerAdded__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void triggerRemoved__wrappee__base( Database db){ secDb.clearForeignKeyTrigger(); }

	 public void triggerRemoved( Database db){ t.in(Thread.currentThread().getStackTrace()[1].toString());	triggerRemoved__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void databaseUpdated__wrappee__base( Database db, Locker locker, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { if (newData == null) { secDb.onForeignKeyDelete(locker,priKey); } }

	 public void databaseUpdated( Database db, Locker locker, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	databaseUpdated__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
