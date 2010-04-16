

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

public class Connection 
{
		protected void handleIncomingMessage(String name, Object msg) 
		{
			original(name, msg);
			
			log("Server", content);
		
			
		}
		
		public void run()
		{
				log("Server","Server: New user has joined.");
				original();
				log("Server","Server: User has left.");
		}
		
		protected static void log(String filename, String content) {
		File datei = new File(filename + "_log.txt");

			try {
				FileWriter schreiber = new FileWriter(datei, true);
				BufferedWriter bw = new BufferedWriter(schreiber);
				bw.write(content);
				bw.newLine();
	
				bw.close();
	
			} catch (Exception e) {
				System.out.println("File not found!");
			}
		}




}