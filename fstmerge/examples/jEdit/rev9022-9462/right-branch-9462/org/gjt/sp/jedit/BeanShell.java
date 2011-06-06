

package org.gjt.sp.jedit;


import bsh.*;
import bsh.classpath.ClassManagerImpl;

import java.io.*;
import java.lang.ref.*;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.gui.BeanShellErrorDialog;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class BeanShell
{
	private static final String REQUIRED_VERSION = "2.0b1.1-jedit-1";

	
	
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

		String script = "int[] lines = textArea.getSelectedLines();\n"
			+ "for(int i = 0; i < lines.length; i++)\n"
			+ "{\n"
				+ "line = lines[i];\n"
				+ "index = line - lines[0];\n"
				+ "start = buffer.getLineStartOffset(line);\n"
				+ "end = buffer.getLineEndOffset(line);\n"
				+ "text = buffer.getText(start,end - start - 1);\n"
				+ "newText = " + command + "\n"
				+ "if(newText != null)\n"
				+ "{\n"
					+ "buffer.remove(start,end - start - 1);\n"
					+ "buffer.insert(start,String.valueOf(newText));\n"
				+ "}\n"
			+ "}\n";

		if(view.getMacroRecorder() != null)
			view.getMacroRecorder().record(1,script);

		try
		{
			buffer.beginCompoundEdit();

			BeanShell.eval(view,global,script);
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

	
	
	public static void runScript(View view, String path, Reader in,
		NameSpace namespace)
	{
		try
		{
			_runScript(view,path,in,namespace);
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
		_runScript(view,path,in,ownNamespace
			? new NameSpace(global,"namespace")
			: global);
	} 

	
	
	public static void _runScript(View view, String path, Reader in,
		NameSpace namespace) throws Exception
	{
		Log.log(Log.MESSAGE,BeanShell.class,"Running script " + path);

		Interpreter interp = createInterpreter(namespace);

		VFS vfs = null;
		Object session = null;

		try
		{
			if(in == null)
			{
				Buffer buffer = jEdit.openTemporary(null,
					null,path,false);

				if(!buffer.isLoaded())
					VFSManager.waitForRequests();

				in = new StringReader(buffer.getText(0,
					buffer.getLength()));
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
				
				if(namespace == global)
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
			catch(UtilEvalError e)
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
		

		classManager = new ClassManagerImpl();
		classManager.setClassLoader(new JARClassLoader());

		global = new NameSpace(classManager,
			"jEdit embedded BeanShell interpreter");
		global.importPackage("org.gjt.sp.jedit");
		global.importPackage("org.gjt.sp.jedit.browser");
		global.importPackage("org.gjt.sp.jedit.buffer");
		global.importPackage("org.gjt.sp.jedit.gui");
		global.importPackage("org.gjt.sp.jedit.help");
		global.importPackage("org.gjt.sp.jedit.io");
		global.importPackage("org.gjt.sp.jedit.menu");
		global.importPackage("org.gjt.sp.jedit.msg");
		global.importPackage("org.gjt.sp.jedit.options");
		global.importPackage("org.gjt.sp.jedit.pluginmgr");
		global.importPackage("org.gjt.sp.jedit.print");
		global.importPackage("org.gjt.sp.jedit.search");
		global.importPackage("org.gjt.sp.jedit.syntax");
		global.importPackage("org.gjt.sp.jedit.textarea");
		global.importPackage("org.gjt.sp.util");

		interpForMethods = createInterpreter(global);
	} 

	
	
	static void resetClassManager()
	{
		classManager.reset();
	} 

	

	

	
	private static final Object[] NO_ARGS = new Object[0];
	private static BshClassManager classManager;
	private static Interpreter interpForMethods;
	private static NameSpace global;
	private static boolean running;
	

	
	private static void setupDefaultVariables(NameSpace namespace, View view)
		throws UtilEvalError
	{
		if(view != null)
		{
			EditPane editPane = view.getEditPane();
			namespace.setVariable("view",view, false);
			namespace.setVariable("editPane",editPane, false);
			namespace.setVariable("buffer",editPane.getBuffer(), false);
			namespace.setVariable("textArea",editPane.getTextArea(), false);
			namespace.setVariable("wm",view.getDockableWindowManager(), false);
		}
	} 

	
	private static void resetDefaultVariables(NameSpace namespace)
		throws UtilEvalError
	{
		namespace.setVariable("view",null, false);
		namespace.setVariable("editPane",null, false);
		namespace.setVariable("buffer",null, false);
		namespace.setVariable("textArea",null, false);
		namespace.setVariable("wm",null, false);
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

	
	private static String getVersion()
	{
		try
		{
			return (String)Interpreter.class.getField("VERSION").get(null);
		}
		catch(Exception e)
		{
			return "unknown";
		}
	} 

	

	
	static class CustomClassManager extends ClassManagerImpl
	{
		private LinkedList listeners = new LinkedList();
		private ReferenceQueue refQueue = new ReferenceQueue();

		
		public synchronized void addListener( Listener l )
		{
			listeners.add( new WeakReference( l, refQueue) );

			
			Reference deadref;
			while ( (deadref = refQueue.poll()) != null )
			{
				boolean ok = listeners.remove( deadref );
				if ( ok )
				{
					
				}
				else
				{
					if ( Interpreter.DEBUG ) Interpreter.debug(
						"tried to remove non-existent weak ref: "+deadref);
				}
			}
		}

		public void removeListener( Listener l )
		{
			throw new Error("unimplemented");
		}

		public void reset()
		{
			classLoaderChanged();
		}

		protected synchronized void classLoaderChanged()
		{
			
			clearCaches();
			if (listeners != null)
			{

				for (Iterator iter = listeners.iterator();
				     iter.hasNext(); )
				{
					WeakReference wr = (WeakReference)
						iter.next();
					Listener l = (Listener)wr.get();
					if ( l == null )  
						iter.remove();
					else
						l.classLoaderChanged();
				}
			}
		}
	} 
}
