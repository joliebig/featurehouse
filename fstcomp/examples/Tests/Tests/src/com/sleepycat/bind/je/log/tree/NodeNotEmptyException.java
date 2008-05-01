package com.sleepycat.je.tree; 
import de.ovgu.cide.jakutil.*; 
public  class  NodeNotEmptyException  extends Exception {
	 public static final NodeNotEmptyException NODE_NOT_EMPTY=new NodeNotEmptyException();

	 private NodeNotEmptyException(){ }


}
