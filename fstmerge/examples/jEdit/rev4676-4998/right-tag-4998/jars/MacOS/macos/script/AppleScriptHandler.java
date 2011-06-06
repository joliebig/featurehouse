

package macos.script;


import java.io.*;
import org.gjt.sp.jedit.*;
import macos.*;


public class AppleScriptHandler extends Macros.Handler
{
	
	public boolean accept(String path)
	{
		return ffilter.accept(new File(path));
	} 

	
	public Macros.Macro createMacro(String macroName, String path)
	{
		if (macroName.toLowerCase().endsWith(".scpt"))
			macroName = macroName.substring(0, macroName.length() - 5);
		else if (macroName.toLowerCase().endsWith(".applescript"))
			macroName = macroName.substring(0, macroName.length() - 12);
		return new Macros.Macro(this,macroName,
			Macros.Macro.macroNameToLabel(macroName),path);
	} 

	
	public void runMacro(View view, Macros.Macro macro)
	{
		MacOSActions.runScript(macro.getPath());
	}
	

	
	public void runMacro(View view, Macros.Macro macro, boolean ownNamespace)
	{
		runMacro(view,macro);
	} 

	
	public AppleScriptHandler()
	{
		super("applescript");
		ffilter = new ScriptFilter();
	} 

	
	private FileFilter ffilter;
	
}
