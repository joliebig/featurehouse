package com.sleepycat.je.config; 
import de.ovgu.cide.jakutil.*; 
public  class  BooleanConfigParam  extends ConfigParam {
	 private static final String DEBUG_NAME=BooleanConfigParam.class.getName();

	 BooleanConfigParam( String configName, boolean defaultValue, boolean mutable, String description){ super(configName,Boolean.valueOf(defaultValue).toString(),mutable,description); }

	 public void validateValue( String value) throws IllegalArgumentException { if (!value.trim().equalsIgnoreCase(Boolean.FALSE.toString()) && !value.trim().equalsIgnoreCase(Boolean.TRUE.toString())) { throw new IllegalArgumentException(DEBUG_NAME + ": " + value+ " not valid boolean "+ name); } }


}
