package com.sleepycat.je.config; 
import de.ovgu.cide.jakutil.*; 
public  class  ConfigParam {
	 public static final String CONFIG_DELIM=";";

	 String name;

	 private String defaultValue;

	 private String description;

	 private boolean mutable;

	 ConfigParam( String configName, String configDefault, boolean mutable, String description) throws IllegalArgumentException { name=configName; defaultValue=configDefault; this.mutable=mutable; this.description=description; validateName(configName); validateValue(configDefault); EnvironmentParams.addSupportedParam(this); }

	 public String getName(){ return name; }

	 public String getDescription(){ return description; }

	 public String getExtraDescription(){ return null; }

	 public String getDefault(){ return defaultValue; }

	 public boolean isMutable(){ return mutable; }

	 public void validate() throws IllegalArgumentException { validateName(name); validateValue(defaultValue); }

	 private void validateName( String name) throws IllegalArgumentException { if ((name == null) || (name.length() < 1)) { throw new IllegalArgumentException(" A configuration parameter" + " name can't be null or 0" + " length"); } }

	 public void validateValue( String value) throws IllegalArgumentException { }

	 public String toString(){ return name; }


}
