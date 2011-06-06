

package org.gjt.sp.jedit.bsh;

import java.util.Vector;


public class CallStack 
{
	private Vector<NameSpace> stack = new Vector<NameSpace>(2);

	public CallStack() { }

	public CallStack( NameSpace namespace ) { 
		push( namespace );
	}

	public void clear() {
		stack.removeAllElements();
	}

	public void push( NameSpace ns ) {
		stack.insertElementAt( ns, 0 );
	}

	public NameSpace top() {
		return get(0);
	}

	
	public NameSpace get(int depth) {
		if ( depth >= depth() )
			return NameSpace.JAVACODE;
		else
			return (NameSpace)(stack.elementAt(depth));
	}
	
	
	public void set(int depth, NameSpace ns) {
		stack.setElementAt(ns, depth );
	}

	public NameSpace pop() {
		if ( depth() < 1 )
			throw new InterpreterError("pop on empty CallStack");
		NameSpace top = top();
		stack.removeElementAt(0);
		return top;
	}

	
	public NameSpace swap( NameSpace newTop ) {
		NameSpace oldTop = (NameSpace)(stack.elementAt(0));
		stack.setElementAt( newTop, 0 );
		return oldTop;
	}

	public int depth() {
		return stack.size();
	}

	public NameSpace [] toArray() {
		NameSpace [] nsa = new NameSpace [ depth() ];
		stack.copyInto( nsa );
		return nsa;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("CallStack:\n");
		NameSpace [] nsa = toArray();
		for(int i=0; i<nsa.length; i++)
			sb.append("\t"+nsa[i]+"\n");

		return sb.toString();
	}

	
	@SuppressWarnings("unchecked")
	public CallStack copy() {
		CallStack cs = new CallStack();
		cs.stack = (Vector<NameSpace>) stack.clone();
		return cs;
	}
}
