package com.sleepycat.je.config; 
import de.ovgu.cide.jakutil.*; 
public  class  ShortConfigParam  extends ConfigParam {
	 private static final String DEBUG_NAME=ShortConfigParam.class.getName();

	 private Short min;

	 private Short max;

	 ShortConfigParam( String configName, Short minVal, Short maxVal, Short defaultValue, boolean mutable, String description){ super(configName,defaultValue.toString(),mutable,description); min=minVal; max=maxVal; }

	 private void validate__wrappee__base( Short value) throws IllegalArgumentException { if (value != null) { if (min != null) { if (value.compareTo(min) < 0) { throw new IllegalArgumentException(DEBUG_NAME + ":" + " param "+ name+ " doesn't validate, "+ value+ " is less than min of "+ min); } } if (max != null) { if (value.compareTo(max) > 0) { throw new IllegalArgumentException(DEBUG_NAME + ":" + " param "+ name+ " doesn't validate, "+ value+ " is greater than max of "+ max); } } } }

	 private void validate( Short value) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void validateValue__wrappee__base( String value) throws IllegalArgumentException { try { validate(new Short(value)); } catch ( NumberFormatException e) { throw new IllegalArgumentException(DEBUG_NAME + ": " + value+ " not valid value for "+ name); } }

	 public void validateValue( String value) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateValue__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getExtraDescription__wrappee__base(){ StringBuffer minMaxDesc=new StringBuffer(); if (min != null) { minMaxDesc.append("# minimum = ").append(min); } if (max != null) { if (min != null) { minMaxDesc.append("\n"); } minMaxDesc.append("# maximum = ").append(max); } return minMaxDesc.toString(); }

	 public String getExtraDescription(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getExtraDescription__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
