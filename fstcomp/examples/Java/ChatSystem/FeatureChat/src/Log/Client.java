

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

public class Client 
{
	public void fireAddLine(String sender, String line, Color color) {
		
		original(sender, line,color);
		log(name, sender.concat(line));

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