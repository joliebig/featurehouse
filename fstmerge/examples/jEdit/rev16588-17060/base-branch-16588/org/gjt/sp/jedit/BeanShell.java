

package org.gjt.sp.jedit;


import org.gjt.sp.jedit.bsh.*;

import java.io.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.gui.BeanShellErrorDialog;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class BeanShell
{
	private static final BeanShellFacade<View> bsh = new MyBeanShellFacade();
	
	static void init()
	{
		Log.log(Log.MESSAGE, BeanShell.class, "Beanshell Init");
	}

	
	
	public static void evalSelection(View view, JEditTextArea textArea)
	{
		bsh.evalSelection(view, textArea);
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
					returnValue = bsh._eval(view,bsh.getNameSpace(),command);
				}
			}
			catch(Throwable e)
			{
				Log.log(Log.ERROR,BeanShell.class,e);

				bsh.handleException(view,null,e);
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

			bsh.eval(view,script);
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

			bsh.handleException(view,path,e);
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

			bsh.handleException(view,path,e);
		}
	} 

	
	
	public static void _runScript(View view, String path, Reader in,
		boolean ownNamespace) throws Exception
	{
		_runScript(view,path,in,ownNamespace
			? new NameSpace(bsh.getNameSpace(),"namespace")
			: bsh.getNameSpace());
	} 

	
	
	public static void _runScript(View view, String path, Reader in,
		NameSpace namespace) throws Exception
	{
		Log.log(Log.MESSAGE,BeanShell.class,"Running script " + path);

		Interpreter interp = BeanShellFacade.createInterpreter(namespace);

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

			bsh.setupDefaultVariables(namespace,view);
			interp.set("scriptPath",path);

			running = true;

			interp.eval(in,namespace,path);
		}
		catch(Exception e)
		{
			BeanShellFacade.unwrapException(e);
		}
		finally
		{
			running = false;
			try
			{
				
				if(namespace == bsh.getNameSpace())
				{
					bsh.resetDefaultVariables(namespace);
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
		return bsh.eval(view, namespace, command);
	} 

	
	
	public static Object _eval(View view, NameSpace namespace, String command)
		throws Exception
	{
		return bsh._eval(view, namespace, command);
	} 

	
	
	public static BshMethod cacheBlock(String id, String code, boolean namespace)
		throws Exception
	{
		return bsh.cacheBlock(id, code, namespace);
	} 

	
	
	public static Object runCachedBlock(BshMethod method, View view,
		NameSpace namespace) throws Exception
	{
		return bsh.runCachedBlock(method, view, namespace);
	} 

	
	
	public static boolean isScriptRunning()
	{
		return running;
	} 

	
	
	public static NameSpace getNameSpace()
	{
		return bsh.getNameSpace();
	} 

	

	
	
	@Deprecated
	public static void runScript(View view, String path,
		boolean ownNamespace, boolean rethrowBshErrors)
	{
		runScript(view,path,null,ownNamespace);
	} 

	
	
	@Deprecated
	public static void runScript(View view, String path, Reader in,
		boolean ownNamespace, boolean rethrowBshErrors)
	{
		runScript(view,path,in,ownNamespace);
	} 

	
	
	@Deprecated
	public static Object eval(View view, String command,
		boolean rethrowBshErrors)
	{
		return bsh.eval(view,command);
	} 

	
	
	@Deprecated
	public static Object eval(View view, NameSpace namespace,
		String command, boolean rethrowBshErrors)
	{
		return eval(view,namespace,command);
	} 

	

	

	
	
	static void resetClassManager()
	{
		bsh.resetClassManager();
	} 

	

	

	
	private static boolean running;
	

	

	
	private static class MyBeanShellFacade extends BeanShellFacade<View>
	{
		private MyBeanShellFacade()
		{
			classManager.setClassLoader(new JARClassLoader());
		}

		@Override
		protected void init()
		{
			super.init();
			global.importPackage("org.gjt.sp.jedit.browser");
			global.importPackage("org.gjt.sp.jedit.bufferset");
			global.importPackage("org.gjt.sp.jedit.statusbar");
			global.importPackage("org.gjt.sp.jedit.gui");
			global.importPackage("org.gjt.sp.jedit.help");
			global.importPackage("org.gjt.sp.jedit.io");
			global.importPackage("org.gjt.sp.jedit.menu");
			global.importPackage("org.gjt.sp.jedit.msg");
			global.importPackage("org.gjt.sp.jedit.options");
			global.importPackage("org.gjt.sp.jedit.pluginmgr");
			global.importPackage("org.gjt.sp.jedit.print");
			global.importPackage("org.gjt.sp.jedit.search");
		}
		
		@Override
		protected void setupDefaultVariables(NameSpace namespace, View view) throws UtilEvalError 
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

		@Override
		protected void resetDefaultVariables(NameSpace namespace) throws UtilEvalError
		{
			namespace.setVariable("view",null, false);
			namespace.setVariable("editPane",null, false);
			namespace.setVariable("buffer",null, false);
			namespace.setVariable("textArea",null, false);
			namespace.setVariable("wm",null, false);
		}

		@Override
		protected void handleException(View view, String path, Throwable t)
		{
			if(t instanceof IOException)
			{
				VFSManager.error(view,path,"ioerror.read-error",
					new String[] { t.toString() });
			}
			else
				new BeanShellErrorDialog(view,t);
		}
		
	}
}
