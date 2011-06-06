

package bsh;


public class BshMethod implements java.io.Serializable 
{
	BSHMethodDeclaration method;

	
	NameSpace declaringNameSpace;

	private Class [] argTypes;

	BshMethod( 
		BSHMethodDeclaration method, NameSpace declaringNameSpace ) 
	{
		this.method = method;
		this.declaringNameSpace = declaringNameSpace;
		
	}

	
	public Class [] getArgTypes() {
		if ( argTypes == null )
			
			argTypes = method.params.argTypes ;

		return argTypes;
	}

	public String getName() {
		return method.name;
	}

	
	public Object invoke( 
		Object[] argValues, Interpreter interpreter, CallStack callstack ) 
		throws EvalError 
	{
		return invokeDeclaredMethod( argValues, interpreter, callstack, null );
	}

	
	Object invokeDeclaredMethod( 
		Object[] argValues, Interpreter interpreter, CallStack callstack,
			SimpleNode callerInfo ) 
		throws EvalError 
	{
		if ( argValues == null )
			argValues = new Object [] { };

		
		if ( argValues.length != method.params.numArgs ) {
			
			try {
				
				String help = 
					(String)declaringNameSpace.get(
					"bsh.help."+method.name, interpreter );

				interpreter.println(help);
				return Primitive.VOID;
			} catch ( Exception e ) {
				throw new EvalError( 
					"Wrong number of arguments for local method: " 
					+ method.name, callerInfo);
			}
		}

		
		NameSpace localNameSpace = new NameSpace( 
			declaringNameSpace, method.name );
		localNameSpace.setNode( callerInfo );

		
		for(int i=0; i<method.params.numArgs; i++)
		{
			
			if ( method.params.argTypes[i] != null ) 
			{
				try {
					argValues[i] = NameSpace.getAssignableForm(argValues[i],
					    method.params.argTypes[i]);
				}
				catch(EvalError e) {
					throw new EvalError(
						"Invalid argument: " 
						+ "`"+method.params.argNames[i]+"'" + " for method: " 
						+ method.name + " : " + 
						e.getMessage(), callerInfo);
				}
				localNameSpace.setTypedVariable( method.params.argNames[i], 
					method.params.argTypes[i], argValues[i], false);
			} 
			
			else  
			{
				
				if ( argValues[i] == Primitive.VOID)
					throw new EvalError(
						"Undefined variable or class name, parameter: " +
						method.params.argNames[i] + " to method: " 
						+ method.name, callerInfo);
				else
					localNameSpace.setVariable(
						method.params.argNames[i], argValues[i]);
			}
		}

		
		callstack.push( localNameSpace );
		
		Object ret = method.block.eval( callstack, interpreter, true );
		
		callstack.pop();

		if ( ret instanceof ReturnControl )
		{
			ReturnControl rs = (ReturnControl)ret;
			if(rs.kind == rs.RETURN)
				ret = ((ReturnControl)ret).value;
			else 
				
				throw new EvalError("continue or break in method body", method);
		}

		
		

		if(method.returnType != null)
		{
			
			
			if(method.returnType == Primitive.VOID)
				return method.returnType;

			
			try {
				ret = NameSpace.getAssignableForm(
					ret, (Class)method.returnType);
			}
			catch(EvalError e) {
				
				throw new EvalError(
					"Incorrect type returned from method: " 
					+ method.name + e.getMessage(), method);
			}
		}

		return ret;
	}

	public String toString() {
		return "Bsh Method: "+method.name;
	}
}
