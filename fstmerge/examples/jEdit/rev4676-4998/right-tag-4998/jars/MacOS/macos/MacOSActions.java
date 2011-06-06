

package macos;


import java.io.*;
import javax.swing.*;
import com.apple.cocoa.application.*;
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
		if (new File(path).exists())
		{
			try {
				String[] args = {"osascript",path};
				Process proc = Runtime.getRuntime().exec(args);
				BufferedReader r = new BufferedReader(
					new InputStreamReader(proc.getErrorStream()));
				proc.waitFor();
				
				String mesg = new String();
				String line;
				while ((line = r.readLine()) != null)
				{
					if (!line.startsWith("##"))
						mesg += line;
				}
				r.close();
				
				if (proc.exitValue() != 0)
					JOptionPane.showMessageDialog(null,mesg,
						"Script Error",JOptionPane.ERROR_MESSAGE);
			} catch (Exception ex) {}
		}
	} 
}
