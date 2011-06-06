

package macos;


import java.io.*;
import javax.swing.*;
import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;
import org.gjt.sp.jedit.*;


public class MacOSActions
{
	
	public static void showInFinder(String path)
	{
		if (new File(path).exists())
		{
			
			
			NSWorkspace.sharedWorkspace().selectFile(path,path);
		}
	} 
	
	
	public static void runScript(String path)
	{
		
		new ScriptRunner(path).start();
		
	} 
	
	
	static class ScriptRunner extends Thread
	{
		private String path;
		
		public ScriptRunner(String path)
		{
			this.path = path;
		}
		
		public void run()
		{
			File file = new File(path);
			
			if (file.exists())
			{
				try {
					BufferedReader reader = new BufferedReader(new FileReader(file));
					StringBuffer code = new StringBuffer();
					String line;
					
					while ((line = reader.readLine()) != null)
						code.append(line);
					
					NSAppleScript script = new NSAppleScript(code.toString());
					script.compile(new NSMutableDictionary());
					script.execute(new NSMutableDictionary());
				} catch (Exception ex) {}
			}
		}
	} 
}
