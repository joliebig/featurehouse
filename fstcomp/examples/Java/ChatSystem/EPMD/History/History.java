

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class History {

	File logFile = null;
	
	public History(String filename) {
		
		logFile = new File(filename);
		
		// History l�schen, wenn sie schon existiert
		if (logFile.exists())
			logFile.delete();
		
		try {
			// Neue Datei erstellen
			logFile.createNewFile();
		} catch (IOException e) {
			// LogDatei nicht verf�gbar
			logFile = null;
			e.printStackTrace();
		}
	}
	
	/**
	 * Gui, Konsole, History
	 */
	
	// neue Nachricht wurde empfangen
	public void onMessageReceived(String text) {
		if (logFile == null)
			return;
		
		FileWriter fw;
		try {
			fw = new FileWriter(logFile, true /* append */);
			fw.append(text);
			fw.append("\r\n");
			fw.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}