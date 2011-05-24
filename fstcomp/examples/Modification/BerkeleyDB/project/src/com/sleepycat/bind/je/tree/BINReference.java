package com.sleepycat.je.tree; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Set; 
import com.sleepycat.je.dbi.DatabaseId; 
import de.ovgu.cide.jakutil.*; 
public  class  BINReference {
	 protected byte[] idKey;

	 private long nodeId;

	 private DatabaseId databaseId;

	 private Set deletedKeys;

	 BINReference( long nodeId, DatabaseId databaseId, byte[] idKey){ this.nodeId=nodeId; this.databaseId=databaseId; this.idKey=idKey; }

	 public long getNodeId__wrappee__base(){ return nodeId; }

	 public long getNodeId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getDatabaseId__wrappee__base(){ return databaseId; }

	 public DatabaseId getDatabaseId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabaseId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getKey__wrappee__base(){ return idKey; }

	 public byte[] getKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getData__wrappee__base(){ return null; }

	 public byte[] getData(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addDeletedKey__wrappee__base( Key key){ if (deletedKeys == null) { deletedKeys=new HashSet(); } deletedKeys.add(key); }

	 public void addDeletedKey( Key key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addDeletedKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addDeletedKeys__wrappee__base( BINReference other){ if (deletedKeys == null) { deletedKeys=new HashSet(); } if (other.deletedKeys != null) { deletedKeys.addAll(other.deletedKeys); } }

	 public void addDeletedKeys( BINReference other){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addDeletedKeys__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void removeDeletedKey__wrappee__base( Key key){ if (deletedKeys != null) { deletedKeys.remove(key); if (deletedKeys.size() == 0) { deletedKeys=null; } } }

	 public void removeDeletedKey( Key key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	removeDeletedKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean hasDeletedKey__wrappee__base( Key key){ return (deletedKeys != null) && deletedKeys.contains(key); }

	 public boolean hasDeletedKey( Key key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasDeletedKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean deletedKeysExist__wrappee__base(){ return ((deletedKeys != null) && (deletedKeys.size() > 0)); }

	 public boolean deletedKeysExist(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	deletedKeysExist__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Iterator getDeletedKeyIterator__wrappee__base(){ if (deletedKeys != null) { return deletedKeys.iterator(); } else { return null; } }

	 public Iterator getDeletedKeyIterator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDeletedKeyIterator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean equals__wrappee__base( Object obj){ if (this == obj) { return true; } if (!(obj instanceof BINReference)) { return false; } return ((BINReference)obj).nodeId == nodeId; }

	 public boolean equals( Object obj){ t.in(Thread.currentThread().getStackTrace()[1].toString());	equals__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int hashCode__wrappee__base(){ return (int)nodeId; }

	 public int hashCode(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hashCode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return "idKey=" + Key.getNoFormatString(idKey) + " nodeId = "+ nodeId+ " db="+ databaseId+ " deletedKeys="+ deletedKeys; }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
