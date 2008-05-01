package com.sleepycat.je.tree; 
import de.ovgu.cide.jakutil.*; 
public  class  CursorsExistException  extends Exception {
	 public static final CursorsExistException CURSORS_EXIST=new CursorsExistException();

	 private CursorsExistException(){ }


}
