

package org.gjt.sp.jedit;


import java.lang.reflect.InvocationTargetException;
import org.gjt.sp.jedit.bsh.BshClassManager;
import org.gjt.sp.jedit.bsh.BshMethod;
import org.gjt.sp.jedit.bsh.CallStack;
import org.gjt.sp.jedit.bsh.Interpreter;
import org.gjt.sp.jedit.bsh.NameSpace;
import org.gjt.sp.jedit.bsh.Primitive;
import org.gjt.sp.jedit.bsh.TargetError;
import org.gjt.sp.jedit.bsh.UtilEvalError;
import org.gjt.sp.jedit.bsh.classpath.ClassManagerImpl;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.util.Log;



public abstract class BeanShellFacade<T>
{
	
	protected BeanShellFacade()
	{
		classManager = new ClassManagerImpl();
		global = new NameSpace(classManager,
			"jEdit embedded BeanShell interpreter");

		interpForMethods = createInterpreter(global);
		init();
	} 

	
	
	protected void init()
	{
		global.importPackage("org.gjt.sp.jedit");
		global.importPackage("org.gjt.sp.jedit.buffer");
		global.importPackage("org.gjt.sp.jedit.syntax");
		global.importPackage("org.gjt.sp.jedit.textarea");
		global.importPackage("org.gjt.sp.util");
	} 

	
	
	public void evalSelection(T param, TextArea textArea)
	{
		String command = textArea.getSelectedText();
		if(command == null)
		{
			textArea.getToolkit().beep();
			return;
		}
		Object returnValue = eval(param,global,command);
		if(returnValue != null)
			textArea.setSelectedText(returnValue.toString());
	} 

	
	
	public Object eval(T param, String command)
	{
		return eval(param, global, command);
	} 

	
	
	public Object eval(T param, NameSpace namespace, String command)
	{
		try
		{
			return _eval(param,namespace,command);
		}
		catch(Throwable e)
		{
			Log.log(Log.ERROR,BeanShellFacade.class,e);

			handleException(param,null,e);
		}

		return null;
	} 

	
	
	public Object _eval(T view, NameSpace namespace, String command)
		throws Exception
	{
		Interpreter interp = createInterpreter(namespace);

		try
		{
			setupDefaultVariables(namespace,view);
			if(Debug.BEANSHELL_DEBUG)
				Log.log(Log.DEBUG,BeanShellFacade.class,command);
			return interp.eval(command);
		}
		catch(Exception e)
		{
			unwrapException(e);
			
			return null;
		}
		finally
		{
			try
			{
				resetDefaultVariables(namespace);
			}
			catch(UtilEvalError e)
			{
				
			}
		}
	} 

	
	
	public BshMethod cacheBlock(String id, String code, boolean namespace)
		throws Exception
	{
		
		
		NameSpace local = new NameSpace(global, "__internal_" + id);
		
		
		String name = "__runCachedMethod";
		if(namespace)
		{
			_eval(null,local,name + "(ns) {\nthis.callstack.set(0,ns);\n" + code + "\n}");
			return local.getMethod(name,new Class[] { NameSpace.class });
		}
		else
		{
			_eval(null,local,name + "() {\n" + code + "\n}");
			return local.getMethod(name,new Class[0]);
		}
	} 

	
	
	public Object runCachedBlock(BshMethod method, T param,
		NameSpace namespace) throws Exception
	{
		boolean useNamespace;
		if(namespace == null)
		{
			useNamespace = false;
			namespace = global;
		}
		else
			useNamespace = true;

		try
		{
			setupDefaultVariables(namespace,param);

			Object retVal = method.invoke(useNamespace
				? new Object[] { namespace }
				: NO_ARGS,
				interpForMethods,new CallStack(), null);
			if(retVal instanceof Primitive)
			{
				if(retVal == Primitive.VOID)
					return null;
				else
					return ((Primitive)retVal).getValue();
			}
			else
				return retVal;
		}
		catch(Exception e)
		{
			unwrapException(e);
			
			return null;
		}
		finally
		{
			resetDefaultVariables(namespace);
		}
	} 

	
	
	public NameSpace getNameSpace()
	{
		return global;
	} 

	
	
	void resetClassManager()
	{
		classManager.reset();
	} 

	
	protected abstract void setupDefaultVariables(NameSpace namespace, T param)
		throws UtilEvalError;
	

	
	protected abstract void resetDefaultVariables(NameSpace namespace)
		throws UtilEvalError;
	

	
	protected abstract void handleException(T param, String path, Throwable t);
	

	
	protected static Interpreter createInterpreter(NameSpace nameSpace)
	{
		return new Interpreter(null,System.out,System.err,false,nameSpace);
	} 

	
	
	protected static void unwrapException(Exception e) throws Exception
	{
		if(e instanceof TargetError)
		{
			Throwable t = ((TargetError)e).getTarget();
			if(t instanceof Exception)
				throw (Exception)t;
			else if(t instanceof Error)
				throw (Error)t;
		}

		if(e instanceof InvocationTargetException)
		{
			Throwable t = ((InvocationTargetException)e).getTargetException();
			if(t instanceof Exception)
				throw (Exception)t;
			else if(t instanceof Error)
				throw (Error)t;
		}

		throw e;
	} 

	
	protected NameSpace global;
	protected BshClassManager classManager;
	private static Interpreter interpForMethods;
	private static final Object[] NO_ARGS = new Object[0];
	
}
