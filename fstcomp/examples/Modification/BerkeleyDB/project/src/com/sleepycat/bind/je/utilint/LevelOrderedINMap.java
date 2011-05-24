package com.sleepycat.je.utilint; 
import java.util.HashSet; 
import java.util.Set; 
import java.util.TreeMap; 
import com.sleepycat.je.tree.IN; 
import de.ovgu.cide.jakutil.*; 
public  class  LevelOrderedINMap  extends TreeMap {
	 public void putIN__wrappee__base( IN in){ Integer level=new Integer(in.getLevel()); Set inSet=(Set)get(level); if (inSet == null) { inSet=new HashSet(); put(level,inSet); } inSet.add(in); }

	 public void putIN( IN in){ t.in(Thread.currentThread().getStackTrace()[1].toString());	putIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
