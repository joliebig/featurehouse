


package bsh;

class BSHMethodInvocation extends SimpleNode
{
	BSHMethodInvocation (int id) { super(id); }

	
	public Object eval( CallStack callstack, Interpreter interpreter)
		throws EvalError
	{
		NameSpace namespace = callstack.top();
		Name name = ((BSHAmbiguousName)jjtGetChild(0)).getName(namespace);
		Object[] args = 
			((BSHArguments)jjtGetChild(1)).getArguments(callstack, interpreter);
		try {
			return name.invokeMethod(interpreter, args, callstack, this);
		} catch (ReflectError e) {
			throw new EvalError(
				"Error in method invocation: " + e.getMessage(), this);
		} catch (java.lang.reflect.InvocationTargetException e) 
		{
			String msg = "Method Invocation "+name;
			Throwable te = e.getTargetException();

			
			boolean isNative = true;
			if ( te instanceof EvalError ) 
				if ( te instanceof TargetError )
					isNative = ((TargetError)te).inNativeCode();
				else
					isNative = false;
			
			throw new TargetError( msg, te, this, isNative );

		} catch ( EvalError ee ) {
			ee.reThrow( this );
			throw new Error("should be unreachable...");
		}

	}
}

