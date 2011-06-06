

package org.gjt.sp.jedit.bsh;

public class DelayedEvalBshMethod extends BshMethod
{
	String returnTypeDescriptor;
	BSHReturnType returnTypeNode;
	String [] paramTypeDescriptors;
	BSHFormalParameters paramTypesNode;

	
	transient CallStack callstack;
	transient Interpreter interpreter;

	
	
	DelayedEvalBshMethod( 
		String name, 
		String returnTypeDescriptor, BSHReturnType returnTypeNode,
		String [] paramNames,
		String [] paramTypeDescriptors, BSHFormalParameters paramTypesNode,
		BSHBlock methodBody, 
		NameSpace declaringNameSpace, Modifiers modifiers,
		CallStack callstack, Interpreter interpreter
	) {
		super( name, null, paramNames, null,
			methodBody, declaringNameSpace, modifiers );

		this.returnTypeDescriptor = returnTypeDescriptor;
		this.returnTypeNode = returnTypeNode;
		this.paramTypeDescriptors = paramTypeDescriptors;
		this.paramTypesNode = paramTypesNode;
		this.callstack = callstack;
		this.interpreter = interpreter;
	}

	public String getReturnTypeDescriptor() { return returnTypeDescriptor; }

	public Class getReturnType() 
	{ 
		if ( returnTypeNode == null )
			return null;

		
		try {
			return returnTypeNode.evalReturnType( callstack, interpreter );
		} catch ( EvalError e ) {
			throw new InterpreterError("can't eval return type: "+e);
		}
	}

	public String [] getParamTypeDescriptors() { return paramTypeDescriptors; }

	public Class [] getParameterTypes() 
	{ 
		
		try {
			return (Class [])paramTypesNode.eval( callstack, interpreter );
		} catch ( EvalError e ) {
			throw new InterpreterError("can't eval param types: "+e);
		}
	}
}
