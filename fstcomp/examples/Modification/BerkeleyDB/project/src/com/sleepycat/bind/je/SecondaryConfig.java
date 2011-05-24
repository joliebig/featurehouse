package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  SecondaryConfig  extends DatabaseConfig {
	 static SecondaryConfig DEFAULT=new SecondaryConfig();

	 private boolean allowPopulate;

	 private SecondaryKeyCreator keyCreator;

	 private SecondaryMultiKeyCreator multiKeyCreator;

	 private Database foreignKeyDatabase;

	 private ForeignKeyDeleteAction foreignKeyDeleteAction=ForeignKeyDeleteAction.ABORT;

	 private ForeignKeyNullifier foreignKeyNullifier;

	 private ForeignMultiKeyNullifier foreignMultiKeyNullifier;

	 private boolean immutableSecondaryKey;

	 public SecondaryConfig(){ }

	 public void setKeyCreator__wrappee__base( SecondaryKeyCreator keyCreator){ this.keyCreator=keyCreator; }

	 public void setKeyCreator( SecondaryKeyCreator keyCreator){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setKeyCreator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public SecondaryKeyCreator getKeyCreator__wrappee__base(){ return keyCreator; }

	 public SecondaryKeyCreator getKeyCreator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKeyCreator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setMultiKeyCreator__wrappee__base( SecondaryMultiKeyCreator multiKeyCreator){ this.multiKeyCreator=multiKeyCreator; }

	 public void setMultiKeyCreator( SecondaryMultiKeyCreator multiKeyCreator){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setMultiKeyCreator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public SecondaryMultiKeyCreator getMultiKeyCreator__wrappee__base(){ return multiKeyCreator; }

	 public SecondaryMultiKeyCreator getMultiKeyCreator(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMultiKeyCreator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setAllowPopulate__wrappee__base( boolean allowPopulate){ this.allowPopulate=allowPopulate; }

	 public void setAllowPopulate( boolean allowPopulate){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setAllowPopulate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getAllowPopulate__wrappee__base(){ return allowPopulate; }

	 public boolean getAllowPopulate(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAllowPopulate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setForeignKeyDatabase__wrappee__base( Database foreignKeyDatabase){ this.foreignKeyDatabase=foreignKeyDatabase; }

	 public void setForeignKeyDatabase( Database foreignKeyDatabase){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setForeignKeyDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Database getForeignKeyDatabase__wrappee__base(){ return foreignKeyDatabase; }

	 public Database getForeignKeyDatabase(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getForeignKeyDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setForeignKeyDeleteAction__wrappee__base( ForeignKeyDeleteAction foreignKeyDeleteAction){ DatabaseUtil.checkForNullParam(foreignKeyDeleteAction,"foreignKeyDeleteAction"); this.foreignKeyDeleteAction=foreignKeyDeleteAction; }

	 public void setForeignKeyDeleteAction( ForeignKeyDeleteAction foreignKeyDeleteAction){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setForeignKeyDeleteAction__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public ForeignKeyDeleteAction getForeignKeyDeleteAction__wrappee__base(){ return foreignKeyDeleteAction; }

	 public ForeignKeyDeleteAction getForeignKeyDeleteAction(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getForeignKeyDeleteAction__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setForeignKeyNullifier__wrappee__base( ForeignKeyNullifier foreignKeyNullifier){ this.foreignKeyNullifier=foreignKeyNullifier; }

	 public void setForeignKeyNullifier( ForeignKeyNullifier foreignKeyNullifier){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setForeignKeyNullifier__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public ForeignKeyNullifier getForeignKeyNullifier__wrappee__base(){ return foreignKeyNullifier; }

	 public ForeignKeyNullifier getForeignKeyNullifier(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getForeignKeyNullifier__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setForeignMultiKeyNullifier__wrappee__base( ForeignMultiKeyNullifier foreignMultiKeyNullifier){ this.foreignMultiKeyNullifier=foreignMultiKeyNullifier; }

	 public void setForeignMultiKeyNullifier( ForeignMultiKeyNullifier foreignMultiKeyNullifier){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setForeignMultiKeyNullifier__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public ForeignMultiKeyNullifier getForeignMultiKeyNullifier__wrappee__base(){ return foreignMultiKeyNullifier; }

	 public ForeignMultiKeyNullifier getForeignMultiKeyNullifier(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getForeignMultiKeyNullifier__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setImmutableSecondaryKey__wrappee__base( boolean immutableSecondaryKey){ this.immutableSecondaryKey=immutableSecondaryKey; }

	 public void setImmutableSecondaryKey( boolean immutableSecondaryKey){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setImmutableSecondaryKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getImmutableSecondaryKey__wrappee__base(){ return immutableSecondaryKey; }

	 public boolean getImmutableSecondaryKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getImmutableSecondaryKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void validate__wrappee__base( DatabaseConfig configArg) throws DatabaseException { super.validate(configArg); if (configArg == null || !(configArg instanceof SecondaryConfig)) { throw new DatabaseException("The SecondaryConfig argument is null."); } SecondaryConfig config=(SecondaryConfig)configArg; boolean kcMatch=equalOrBothNull(config.getKeyCreator(),keyCreator); boolean mkcMatch=equalOrBothNull(config.getMultiKeyCreator(),multiKeyCreator); boolean fkdMatch=(config.getForeignKeyDatabase() == foreignKeyDatabase); boolean fkdaMatch=(config.getForeignKeyDeleteAction() == foreignKeyDeleteAction); boolean fknMatch=equalOrBothNull(config.getForeignKeyNullifier(),foreignKeyNullifier); boolean fmknMatch=equalOrBothNull(config.getForeignMultiKeyNullifier(),foreignMultiKeyNullifier); boolean imskMatch=(config.getImmutableSecondaryKey() == immutableSecondaryKey); if (kcMatch && mkcMatch && fkdMatch&& fkdaMatch&& fknMatch&& fmknMatch&& imskMatch) { return; } String message=genSecondaryConfigMismatchMessage(config,kcMatch,mkcMatch,fkdMatch,fkdaMatch,fknMatch,fmknMatch,imskMatch); throw new DatabaseException(message); }

	 void validate( DatabaseConfig configArg) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean equalOrBothNull__wrappee__base( Object o1, Object o2){ return (o1 != null) ? o1.equals(o2) : (o2 == null); }

	 private boolean equalOrBothNull( Object o1, Object o2){ t.in(Thread.currentThread().getStackTrace()[1].toString());	equalOrBothNull__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 String genSecondaryConfigMismatchMessage__wrappee__base( DatabaseConfig config, boolean kcMatch, boolean mkcMatch, boolean fkdMatch, boolean fkdaMatch, boolean fknMatch, boolean fmknMatch, boolean imskMatch){ StringBuffer ret=new StringBuffer("The following SecondaryConfig parameters for the\n" + "cached Database do not match the parameters for the\n" + "requested Database:\n"); if (!kcMatch) { ret.append(" SecondaryKeyCreator\n"); } if (!mkcMatch) { ret.append(" SecondaryMultiKeyCreator\n"); } if (!fkdMatch) { ret.append(" ForeignKeyDelete\n"); } if (!fkdaMatch) { ret.append(" ForeignKeyDeleteAction\n"); } if (!fknMatch) { ret.append(" ForeignKeyNullifier\n"); } if (!fknMatch) { ret.append(" ForeignMultiKeyNullifier\n"); } if (!imskMatch) { ret.append(" ImmutableSecondaryKey\n"); } return ret.toString(); }

	 String genSecondaryConfigMismatchMessage( DatabaseConfig config, boolean kcMatch, boolean mkcMatch, boolean fkdMatch, boolean fkdaMatch, boolean fknMatch, boolean fmknMatch, boolean imskMatch){ t.in(Thread.currentThread().getStackTrace()[1].toString());	genSecondaryConfigMismatchMessage__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
