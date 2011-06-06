

package org.gjt.sp.jedit.bsh;

public class ClassIdentifier 
{
	Class clas;

	public ClassIdentifier( Class clas ) {
		this.clas = clas;
	}

	
	public Class getTargetClass() {
		return clas;
	}

	public String toString() {
		return "Class Identifier: "+clas.getName();
	}
}

