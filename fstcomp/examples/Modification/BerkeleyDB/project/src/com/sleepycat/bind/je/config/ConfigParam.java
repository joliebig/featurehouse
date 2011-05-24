package com.sleepycat.je.config; 
import de.ovgu.cide.jakutil.*; 
public  class  ConfigParam {
	 public static final String CONFIG_DELIM=";";

	 String name;

	 private String defaultValue;

	 private String description;

	 private boolean mutable;

	 ConfigParam( String configName, String configDefault, boolean mutable, String description) throws IllegalArgumentException { name=configName; defaultValue=configDefault; this.mutable=mutable; this.description=description; validateName(configName); validateValue(configDefault); EnvironmentParams.addSupportedParam(this); }

	 public String getName__wrappee__base(){ return name; }

	 public String getName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getDescription__wrappee__base(){ return description; }

	 public String getDescription(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDescription__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getExtraDescription__wrappee__base(){ return null; }

	 public String getExtraDescription(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getExtraDescription__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getDefault__wrappee__base(){ return defaultValue; }

	 public String getDefault(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDefault__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isMutable__wrappee__base(){ return mutable; }

	 public boolean isMutable(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isMutable__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void validate__wrappee__base() throws IllegalArgumentException { validateName(name); validateValue(defaultValue); }

	 public void validate() throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void validateName__wrappee__base( String name) throws IllegalArgumentException { if ((name == null) || (name.length() < 1)) { throw new IllegalArgumentException(" A configuration parameter" + " name can't be null or 0" + " length"); } }

	 private void validateName( String name) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void validateValue__wrappee__base( String value) throws IllegalArgumentException { }

	 public void validateValue( String value) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateValue__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return name; }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
