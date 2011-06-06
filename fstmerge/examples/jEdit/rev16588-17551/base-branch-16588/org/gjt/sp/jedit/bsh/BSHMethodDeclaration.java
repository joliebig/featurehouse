

package org.gjt.sp.jedit.bsh;

class BSHMethodDeclaration extends SimpleNode
{
	public String name;

	

	BSHReturnType returnTypeNode;
	BSHFormalParameters paramsNode;
	BSHBlock blockNode;
	
	int firstThrowsClause;

	

	public Modifiers modifiers;

	
	Class returnType;  
	int numThrows = 0;

	BSHMethodDeclaration(int id) { super(id); }

	
	synchronized void insureNodesParsed() 
	{
		if ( paramsNode != null ) 
			return;

		Object firstNode = jjtGetChild(0);
		firstThrowsClause = 1;
		if ( firstNode instanceof BSHReturnType )
		{
			returnTypeNode = (BSHReturnType)firstNode;
			paramsNode = (BSHFormalParameters)jjtGetChild(1);
			if ( jjtGetNumChildren() > 2+numThrows )
				blockNode = (BSHBlock)jjtGetChild(2+numThrows); 
			++firstThrowsClause;
		}
		else
		{
			paramsNode = (BSHFormalParameters)jjtGetChild(0);
			blockNode = (BSHBlock)jjtGetChild(1+numThrows); 
		}
	}

	
	Class evalReturnType( CallStack callstack, Interpreter interpreter )
		throws EvalError
	{
		insureNodesParsed();
		if ( returnTypeNode != null )
			return returnTypeNode.evalReturnType( callstack, interpreter );
		else 
			return null;
	}

	String getReturnTypeDescriptor( 
		CallStack callstack, Interpreter interpreter, String defaultPackage )
	{
		insureNodesParsed();
		if ( returnTypeNode == null )
			return null;
		else
			return returnTypeNode.getTypeDescriptor( 
				callstack, interpreter, defaultPackage );
	}

	BSHReturnType getReturnTypeNode() {
		insureNodesParsed();
		return returnTypeNode;
	}

	
	public Object eval( CallStack callstack, Interpreter interpreter )
		throws EvalError
	{
		returnType = evalReturnType( callstack, interpreter );
		evalNodes( callstack, interpreter );

		
		






		NameSpace namespace = callstack.top();
		BshMethod bshMethod = new BshMethod( this, namespace, modifiers );
		try {
			namespace.setMethod( name, bshMethod );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError(this,callstack);
		}

		return Primitive.VOID;
	}

	private void evalNodes( CallStack callstack, Interpreter interpreter ) 
		throws EvalError
	{
		insureNodesParsed();
		
		
		for(int i=firstThrowsClause; i<numThrows+firstThrowsClause; i++)
			((BSHAmbiguousName)jjtGetChild(i)).toClass( 
				callstack, interpreter );

		paramsNode.eval( callstack, interpreter );

		
		if ( interpreter.getStrictJava() )
		{
			for(int i=0; i<paramsNode.paramTypes.length; i++)
				if ( paramsNode.paramTypes[i] == null )
					
					
					throw new EvalError(
				"(Strict Java Mode) Undeclared argument type, parameter: " +
					paramsNode.getParamNames()[i] + " in method: " 
					+ name, this, null );

			if ( returnType == null )
				
				
				throw new EvalError(
				"(Strict Java Mode) Undeclared return type for method: "
					+ name, this, null );
		}
	}

	public String toString() {
		return "MethodDeclaration: "+name;
	}
}
