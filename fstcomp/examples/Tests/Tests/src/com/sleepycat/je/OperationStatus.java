package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  class  OperationStatus {
	 public static final OperationStatus SUCCESS=new OperationStatus("SUCCESS");

	 public static final OperationStatus KEYEXIST=new OperationStatus("KEYEXIST");

	 public static final OperationStatus KEYEMPTY=new OperationStatus("KEYEMPTY");

	 public static final OperationStatus NOTFOUND=new OperationStatus("NOTFOUND");

	 private String statusName;

	 private OperationStatus( String statusName){ this.statusName=statusName; }

	 public String toString(){ return "OperationStatus." + statusName; }


}
