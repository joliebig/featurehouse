package com.sleepycat.je; 
import java.util.Comparator; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import de.ovgu.cide.jakutil.*; 
public  class  DatabaseConfig  implements Cloneable {
	 public static final DatabaseConfig DEFAULT=new DatabaseConfig();

	 private boolean allowCreate=false;

	 private boolean exclusiveCreate=false;

	 private boolean transactional=false;

	 private boolean readOnly=false;

	 private boolean duplicatesAllowed=false;

	 private int nodeMax;

	 private int nodeMaxDupTree;

	 private Comparator btreeComparator=null;

	 private Comparator duplicateComparator=null;

	 private boolean overrideBtreeComparator=false;

	 private boolean overrideDupComparator=false;

	 private boolean useExistingConfig=false;

	 public DatabaseConfig(){ }

	 public void setAllowCreate__wrappee__base( boolean allowCreate){ this.allowCreate=allowCreate; }

	 public void setAllowCreate( boolean allowCreate){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAllowCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getAllowCreate__wrappee__base(){ return allowCreate; }

	 public boolean getAllowCreate(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAllowCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setExclusiveCreate__wrappee__base( boolean exclusiveCreate){ this.exclusiveCreate=exclusiveCreate; }

	 public void setExclusiveCreate( boolean exclusiveCreate){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setExclusiveCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getExclusiveCreate__wrappee__base(){ return exclusiveCreate; }

	 public boolean getExclusiveCreate(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getExclusiveCreate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setSortedDuplicates__wrappee__base( boolean duplicatesAllowed){ this.duplicatesAllowed=duplicatesAllowed; }

	 public void setSortedDuplicates( boolean duplicatesAllowed){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setSortedDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getSortedDuplicates__wrappee__base(){ return duplicatesAllowed; }

	 public boolean getSortedDuplicates(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSortedDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTransactional__wrappee__base( boolean transactional){ this.transactional=transactional; }

	 public void setTransactional( boolean transactional){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getTransactional__wrappee__base(){ return transactional; }

	 public boolean getTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setReadOnly__wrappee__base( boolean readOnly){ this.readOnly=readOnly; }

	 public void setReadOnly( boolean readOnly){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setReadOnly__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getReadOnly__wrappee__base(){ return readOnly; }

	 public boolean getReadOnly(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getReadOnly__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setNodeMaxEntries__wrappee__base( int nodeMaxEntries){ this.nodeMax=nodeMaxEntries; }

	 public void setNodeMaxEntries( int nodeMaxEntries){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setNodeMaxEntries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setNodeMaxDupTreeEntries__wrappee__base( int nodeMaxDupTreeEntries){ this.nodeMaxDupTree=nodeMaxDupTreeEntries; }

	 public void setNodeMaxDupTreeEntries( int nodeMaxDupTreeEntries){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setNodeMaxDupTreeEntries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getNodeMaxEntries__wrappee__base(){ return nodeMax; }

	 public int getNodeMaxEntries(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNodeMaxEntries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getNodeMaxDupTreeEntries__wrappee__base(){ return nodeMaxDupTree; }

	 public int getNodeMaxDupTreeEntries(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNodeMaxDupTreeEntries__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setBtreeComparator__wrappee__base( Class btreeComparator){ this.btreeComparator=validateComparator(btreeComparator,"Btree"); }

	 public void setBtreeComparator( Class btreeComparator){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setBtreeComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Comparator getBtreeComparator__wrappee__base(){ return btreeComparator; }

	 public Comparator getBtreeComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBtreeComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setOverrideBtreeComparator__wrappee__base( boolean override){ overrideBtreeComparator=override; }

	 public void setOverrideBtreeComparator( boolean override){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setOverrideBtreeComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getOverrideBtreeComparator__wrappee__base(){ return overrideBtreeComparator; }

	 public boolean getOverrideBtreeComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getOverrideBtreeComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setDuplicateComparator__wrappee__base( Class duplicateComparator){ this.duplicateComparator=validateComparator(duplicateComparator,"Duplicate"); }

	 public void setDuplicateComparator( Class duplicateComparator){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDuplicateComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Comparator getDuplicateComparator__wrappee__base(){ return duplicateComparator; }

	 public Comparator getDuplicateComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDuplicateComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setOverrideDuplicateComparator__wrappee__base( boolean override){ overrideDupComparator=override; }

	 public void setOverrideDuplicateComparator( boolean override){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setOverrideDuplicateComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getOverrideDuplicateComparator__wrappee__base(){ return overrideDupComparator; }

	 public boolean getOverrideDuplicateComparator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getOverrideDuplicateComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setUseExistingConfig__wrappee__base( boolean useExistingConfig){ this.useExistingConfig=useExistingConfig; }

	 void setUseExistingConfig( boolean useExistingConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setUseExistingConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean getUseExistingConfig__wrappee__base(){ return useExistingConfig; }

	 boolean getUseExistingConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getUseExistingConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 DatabaseConfig cloneConfig__wrappee__base(){ try { return (DatabaseConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 DatabaseConfig cloneConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void validate__wrappee__base( DatabaseConfig config) throws DatabaseException { if (config == null) { config=DatabaseConfig.DEFAULT; } boolean txnMatch=config.getTransactional() == transactional; boolean roMatch=config.getReadOnly() == readOnly; boolean sdMatch=config.getSortedDuplicates() == duplicatesAllowed; boolean btCmpMatch=(config.overrideBtreeComparator ? btreeComparator.getClass() == config.getBtreeComparator().getClass() : true); boolean dtCmpMatch=(config.getOverrideDuplicateComparator() ? duplicateComparator.getClass() == config.getDuplicateComparator().getClass() : true); if (txnMatch && roMatch && sdMatch&& btCmpMatch&& dtCmpMatch) { return; } else { String message=genDatabaseConfigMismatchMessage(config,txnMatch,roMatch,sdMatch,btCmpMatch,dtCmpMatch); throw new DatabaseException(message); } }

	 void validate( DatabaseConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 String genDatabaseConfigMismatchMessage__wrappee__base( DatabaseConfig config, boolean txnMatch, boolean roMatch, boolean sdMatch, boolean btCmpMatch, boolean dtCmpMatch){ StringBuffer ret=new StringBuffer("The following DatabaseConfig parameters for the\n" + "cached Database do not match the parameters for the\n" + "requested Database:\n"); if (!txnMatch) { ret.append(" Transactional\n"); } if (!roMatch) { ret.append(" Read-Only\n"); } if (!sdMatch) { ret.append(" Sorted Duplicates\n"); } if (!btCmpMatch) { ret.append(" Btree Comparator\n"); } if (!dtCmpMatch) { ret.append(" Duplicate Comparator\n"); } return ret.toString(); }

	 String genDatabaseConfigMismatchMessage( DatabaseConfig config, boolean txnMatch, boolean roMatch, boolean sdMatch, boolean btCmpMatch, boolean dtCmpMatch){ t.in(Thread.currentThread().getStackTrace()[1].toString());	genDatabaseConfigMismatchMessage__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private Comparator validateComparator__wrappee__base( Class comparator, String type) throws IllegalArgumentException { if (comparator == null) { return null; } try { Comparator ret=DatabaseImpl.instantiateComparator(comparator,type); if (ret instanceof Comparator) { return ret; } else { throw new IllegalArgumentException(comparator.getName() + " is is not valid as a " + type+ " comparator because it does not "+ " implement java.util.Comparator."); } } catch ( DatabaseException e) { throw new IllegalArgumentException(type + " comparator is not valid: " + e.getMessage()+ "\nPerhaps you have not implemented a zero-parameter "+ "constructor for the comparator or the comparator class "+ "cannot be found."); } }

	 private Comparator validateComparator( Class comparator, String type) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
