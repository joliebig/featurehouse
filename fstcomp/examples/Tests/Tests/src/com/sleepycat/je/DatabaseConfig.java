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

	 public void setAllowCreate( boolean allowCreate){ this.allowCreate=allowCreate; }

	 public boolean getAllowCreate(){ return allowCreate; }

	 public void setExclusiveCreate( boolean exclusiveCreate){ this.exclusiveCreate=exclusiveCreate; }

	 public boolean getExclusiveCreate(){ return exclusiveCreate; }

	 public void setSortedDuplicates( boolean duplicatesAllowed){ this.duplicatesAllowed=duplicatesAllowed; }

	 public boolean getSortedDuplicates(){ return duplicatesAllowed; }

	 public void setTransactional( boolean transactional){ this.transactional=transactional; }

	 public boolean getTransactional(){ return transactional; }

	 public void setReadOnly( boolean readOnly){ this.readOnly=readOnly; }

	 public boolean getReadOnly(){ return readOnly; }

	 public void setNodeMaxEntries( int nodeMaxEntries){ this.nodeMax=nodeMaxEntries; }

	 public void setNodeMaxDupTreeEntries( int nodeMaxDupTreeEntries){ this.nodeMaxDupTree=nodeMaxDupTreeEntries; }

	 public int getNodeMaxEntries(){ return nodeMax; }

	 public int getNodeMaxDupTreeEntries(){ return nodeMaxDupTree; }

	 public void setBtreeComparator( Class btreeComparator){ this.btreeComparator=validateComparator(btreeComparator,"Btree"); }

	 public Comparator getBtreeComparator(){ return btreeComparator; }

	 public void setOverrideBtreeComparator( boolean override){ overrideBtreeComparator=override; }

	 public boolean getOverrideBtreeComparator(){ return overrideBtreeComparator; }

	 public void setDuplicateComparator( Class duplicateComparator){ this.duplicateComparator=validateComparator(duplicateComparator,"Duplicate"); }

	 public Comparator getDuplicateComparator(){ return duplicateComparator; }

	 public void setOverrideDuplicateComparator( boolean override){ overrideDupComparator=override; }

	 public boolean getOverrideDuplicateComparator(){ return overrideDupComparator; }

	 void setUseExistingConfig( boolean useExistingConfig){ this.useExistingConfig=useExistingConfig; }

	 boolean getUseExistingConfig(){ return useExistingConfig; }

	 DatabaseConfig cloneConfig(){ try { return (DatabaseConfig)super.clone(); } catch ( CloneNotSupportedException willNeverOccur) { return null; } }

	 void validate( DatabaseConfig config) throws DatabaseException { if (config == null) { config=DatabaseConfig.DEFAULT; } boolean txnMatch=config.getTransactional() == transactional; boolean roMatch=config.getReadOnly() == readOnly; boolean sdMatch=config.getSortedDuplicates() == duplicatesAllowed; boolean btCmpMatch=(config.overrideBtreeComparator ? btreeComparator.getClass() == config.getBtreeComparator().getClass() : true); boolean dtCmpMatch=(config.getOverrideDuplicateComparator() ? duplicateComparator.getClass() == config.getDuplicateComparator().getClass() : true); if (txnMatch && roMatch && sdMatch&& btCmpMatch&& dtCmpMatch) { return; } else { String message=genDatabaseConfigMismatchMessage(config,txnMatch,roMatch,sdMatch,btCmpMatch,dtCmpMatch); throw new DatabaseException(message); } }

	 String genDatabaseConfigMismatchMessage( DatabaseConfig config, boolean txnMatch, boolean roMatch, boolean sdMatch, boolean btCmpMatch, boolean dtCmpMatch){ StringBuffer ret=new StringBuffer("The following DatabaseConfig parameters for the\n" + "cached Database do not match the parameters for the\n" + "requested Database:\n"); if (!txnMatch) { ret.append(" Transactional\n"); } if (!roMatch) { ret.append(" Read-Only\n"); } if (!sdMatch) { ret.append(" Sorted Duplicates\n"); } if (!btCmpMatch) { ret.append(" Btree Comparator\n"); } if (!dtCmpMatch) { ret.append(" Duplicate Comparator\n"); } return ret.toString(); }

	 private Comparator validateComparator( Class comparator, String type) throws IllegalArgumentException { if (comparator == null) { return null; } try { Comparator ret=DatabaseImpl.instantiateComparator(comparator,type); if (ret instanceof Comparator) { return ret; } else { throw new IllegalArgumentException(comparator.getName() + " is is not valid as a " + type+ " comparator because it does not "+ " implement java.util.Comparator."); } } catch ( DatabaseException e) { throw new IllegalArgumentException(type + " comparator is not valid: " + e.getMessage()+ "\nPerhaps you have not implemented a zero-parameter "+ "constructor for the comparator or the comparator class "+ "cannot be found."); } }


}
