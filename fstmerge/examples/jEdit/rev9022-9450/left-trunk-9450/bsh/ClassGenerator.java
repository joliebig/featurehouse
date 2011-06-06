package bsh;

import bsh.Capabilities.Unavailable;
import java.lang.reflect.InvocationTargetException;

public abstract class ClassGenerator
{
	private static ClassGenerator cg;

	public static ClassGenerator getClassGenerator() 
		throws UtilEvalError
	{
		if ( cg == null ) 
		{
			try {
				Class clas = Class.forName( "bsh.ClassGeneratorImpl" );
				cg = (ClassGenerator)clas.newInstance();
			} catch ( Exception e ) {
				throw new Unavailable("ClassGenerator unavailable: "+e);
			}
		}
	
		return cg;
	}

	
	public abstract Class generateClass( 
		String name, Modifiers modifiers, 
		Class [] interfaces, Class superClass, BSHBlock block, 
		boolean isInterface, CallStack callstack, Interpreter interpreter 
	)
		throws EvalError;

	
	public abstract Object invokeSuperclassMethod(
		BshClassManager bcm, Object instance, String methodName, Object [] args
	)
        throws UtilEvalError, ReflectError, InvocationTargetException;

	
	public abstract void setInstanceNameSpaceParent( 
		Object instance, String className, NameSpace parent );

}
