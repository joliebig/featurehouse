

package org.gjt.sp.jedit;


import bsh.*;
import java.lang.reflect.InvocationTargetException;
import java.io.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.gui.BeanShellErrorDialog;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class BeanShell
{
	
	
	public static void evalSelection(View view, JEditTextArea textArea)
	{
		String command = textArea.getSelectedText();
		if(command == null)
		{
			view.getToolkit().beep();
			return;
		}
		Object returnValue = eval(view,global,command);
		if(returnValue != null)
			textArea.setSelectedText(returnValue.toString());
	} 

	
	
	public static void showEvaluateDialog(View view)
	{
		String command = GUIUtilities.input(view,"beanshell-eval-input",null);
		if(command != null)
		{
			if(!command.endsWith(";"))
				command = command + ";";

			int repeat = view.getInputHandler().getRepeatCount();

			if(view.getMacroRecorder() != null)
			{
				view.getMacroRecorder().record(repeat,command);
			}

			Object returnValue = null;
			try
			{
				for(int i = 0; i < repeat; i++)
				{
					returnValue = _eval(view,global,command);
				}
			}
			catch(Throwable e)
			{
				Log.log(Log.ERROR,BeanShell.class,e);

				handleException(view,null,e);
			}

			if(returnValue != null)
			{
				String[] args = { returnValue.toString() };
				GUIUtilities.message(view,"beanshell-eval",args);
			}
		}
	} 

	
	
	public static void showEvaluateLinesDialog(View view)
	{
		String command = GUIUtilities.input(view,"beanshell-eval-line",null);

		JEditTextArea textArea = view.getTextArea();
		Buffer buffer = view.getBuffer();

		if(command == null || command.length() == 0)
			return;

		Selection[] selection = textArea.getSelection();
		if(selection.length == 0)
		{
			view.getToolkit().beep();
			return;
		}

		if(!command.endsWith(";"))
			command = command + ";";

		if(view.getMacroRecorder() != null)
			view.getMacroRecorder().record(1,command);

		try
		{
			buffer.beginCompoundEdit();

			for(int i = 0; i < selection.length; i++)
			{
				Selection s = selection[i];
				for(int j = s.getStartLine(); j <= s.getEndLine(); j++)
				{
					
					
					if(s.getEnd() == textArea.getLineStartOffset(j))
						break;

					global.setVariable("line",new Integer(j));
					global.setVariable("index",new Integer(
						j - s.getStartLine()));
					int start = s.getStart(buffer,j);
					int end = s.getEnd(buffer,j);
					String text = buffer.getText(start,
						end - start);
					global.setVariable("text",text);

					Object returnValue = _eval(view,global,command);
					if(returnValue != null)
					{
						buffer.remove(start,end - start);
						buffer.insert(start,
							returnValue.toString());
					}
				}
			}
		}
		catch(Throwable e)
		{
			Log.log(Log.ERROR,BeanShell.class,e);

			handleException(view,null,e);
		}
		finally
		{
			buffer.endCompoundEdit();
		}

		textArea.selectNone();
	} 

	
	
	public static void runScript(View view, String path, Reader in,
		boolean ownNamespace)
	{
		try
		{
			_runScript(view,path,in,ownNamespace);
		}
		catch(Throwable e)
		{
			Log.log(Log.ERROR,BeanShell.class,e);

			handleException(view,path,e);
		}
	} 

	
	
	public static void _runScript(View view, String path, Reader in,
		boolean ownNamespace) throws Exception
	{
		Log.log(Log.MESSAGE,BeanShell.class,"Running script " + path);

		NameSpace namespace;
		if(ownNamespace)
			namespace = new NameSpace(global,"script namespace");
		else
			namespace = global;

		Interpreter interp = createInterpreter(namespace);

		VFS vfs = null;
		Object session = null;

		try
		{
			if(in == null)
			{
				Buffer buffer = jEdit.getBuffer(path);

				vfs = VFSManager.getVFSForPath(path);
				session = vfs.createVFSSession(path,view);
				if(session == null)
				{
					
					return;
				}

				if(buffer != null)
				{
					if(!buffer.isLoaded())
						VFSManager.waitForRequests();

					in = new StringReader(buffer.getText(0,
						buffer.getLength()));
				}
				else
				{
					in = new BufferedReader(new InputStreamReader(
						vfs._createInputStream(session,
						path,false,view)));
				}
			}

			setupDefaultVariables(namespace,view);
			interp.set("scriptPath",path);

			running = true;

			interp.eval(in,namespace,path);
		}
		catch(Exception e)
		{
			unwrapException(e);
		}
		finally
		{
			running = false;

			if(session != null)
			{
				try
				{
					vfs._endVFSSession(session,view);
				}
				catch(IOException io)
				{
					Log.log(Log.ERROR,BeanShell.class,io);
					GUIUtilities.error(view,"read-error",
						new String[] { path, io.toString() });
				}
			}

			try
			{
				
				if(!ownNamespace)
				{
					resetDefaultVariables(namespace);
					interp.unset("scriptPath");
				}
			}
			catch(EvalError e)
			{
				
			}
		}
	} 

	
	
	public static Object eval(View view, NameSpace namespace, String command)
	{
		try
		{
			return _eval(view,namespace,command);
		}
		catch(Throwable e)
		{
			Log.log(Log.ERROR,BeanShell.class,e);

			handleException(view,null,e);
		}

		return null;
	} 

	
	
	public static Object _eval(View view, NameSpace namespace, String command)
		throws Exception
	{
		Interpreter interp = createInterpreter(namespace);

		try
		{
			setupDefaultVariables(namespace,view);
			if(Debug.BEANSHELL_DEBUG)
				Log.log(Log.DEBUG,BeanShell.class,command);
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
			catch(EvalError e)
			{
				
			}
		}
	} 

	
	
	public static BshMethod cacheBlock(String id, String code, boolean namespace)
		throws Exception
	{
		String name = "__internal_" + id;

		
		if(namespace)
		{
			_eval(null,global,name + "(ns) {\nthis.callstack.set(0,ns);\n" + code + "\n}");
			return global.getMethod(name,new Class[] { NameSpace.class });
		}
		else
		{
			_eval(null,global,name + "() {\n" + code + "\n}");
			return global.getMethod(name,new Class[0]);
		}
	} 

	
	
	public static Object runCachedBlock(BshMethod method, View view,
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
			setupDefaultVariables(namespace,view);

			Object retVal = method.invoke(useNamespace
				? new Object[] { namespace }
				: NO_ARGS,
				interpForMethods,new CallStack());
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

	
	
	public static boolean isScriptRunning()
	{
		return running;
	} 

	
	
	public static NameSpace getNameSpace()
	{
		return global;
	} 

	

	
	
	public static void runScript(View view, String path,
		boolean ownNamespace, boolean rethrowBshErrors)
	{
		runScript(view,path,null,ownNamespace);
	} 

	
	
	public static void runScript(View view, String path, Reader in,
		boolean ownNamespace, boolean rethrowBshErrors)
	{
		runScript(view,path,in,ownNamespace);
	} 

	
	
	public static Object eval(View view, String command,
		boolean rethrowBshErrors)
	{
		return eval(view,global,command);
	} 

	
	
	public static Object eval(View view, NameSpace namespace,
		String command, boolean rethrowBshErrors)
	{
		return eval(view,namespace,command);
	} 

	

	

	
	static void init()
	{
		BshClassManager.setClassLoader(new JARClassLoader());

		global = new NameSpace("jEdit embedded BeanShell interpreter");
		global.importPackage("org.gjt.sp.jedit");
		global.importPackage("org.gjt.sp.jedit.browser");
		global.importPackage("org.gjt.sp.jedit.buffer");
		global.importPackage("org.gjt.sp.jedit.gui");
		global.importPackage("org.gjt.sp.jedit.help");
		global.importPackage("org.gjt.sp.jedit.io");
		global.importPackage("org.gjt.sp.jedit.msg");
		global.importPackage("org.gjt.sp.jedit.options");
		global.importPackage("org.gjt.sp.jedit.pluginmgr");
		global.importPackage("org.gjt.sp.jedit.print");
		global.importPackage("org.gjt.sp.jedit.search");
		global.importPackage("org.gjt.sp.jedit.syntax");
		global.importPackage("org.gjt.sp.jedit.textarea");
		global.importPackage("org.gjt.sp.util");

		interpForMethods = createInterpreter(global);

		Log.log(Log.DEBUG,BeanShell.class,"BeanShell interpreter version "
			+ Interpreter.VERSION);
	} 

	

	

	
	private static final Object[] NO_ARGS = new Object[0];
	private static Interpreter interpForMethods;
	private static NameSpace global;
	private static boolean running;
	

	
	private static void setupDefaultVariables(NameSpace namespace, View view)
		throws EvalError
	{
		if(view != null)
		{
			EditPane editPane = view.getEditPane();
			namespace.setVariable("view",view);
			namespace.setVariable("editPane",editPane);
			namespace.setVariable("buffer",editPane.getBuffer());
			namespace.setVariable("textArea",editPane.getTextArea());
			namespace.setVariable("wm",view.getDockableWindowManager());
		}
	} 

	
	private static void resetDefaultVariables(NameSpace namespace)
		throws EvalError
	{
		namespace.setVariable("view",null);
		namespace.setVariable("editPane",null);
		namespace.setVariable("buffer",null);
		namespace.setVariable("textArea",null);
		namespace.setVariable("wm",null);
	} 

	
	
	private static void unwrapException(Exception e) throws Exception
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

	
	private static void handleException(View view, String path, Throwable t)
	{
		if(t instanceof IOException)
		{
			VFSManager.error(view,path,"ioerror.read-error",
				new String[] { t.toString() });
		}
		else
			new BeanShellErrorDialog(view,t);
	} 

	
	private static Interpreter createInterpreter(NameSpace nameSpace)
	{
		return new Interpreter(null,System.out,System.err,false,nameSpace);
	} 

	
}
