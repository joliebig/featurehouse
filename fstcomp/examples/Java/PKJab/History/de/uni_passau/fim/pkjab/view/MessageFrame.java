
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import pkjab.de.uni_passau.fim.pkjab.model.Contact;

class MessageFrame {
    
    protected static final String HISTORY_DIR = PKjabToolkit.PKJAB_DIR + "/history/";
    
    private FileWriter historyFile;
    
    private void writeToFile(String text) {
    	try {
    		if (historyFile == null) {
    			
    			File file = new File(PKjabToolkit.PKJAB_DIR);
    			if (!file.exists()) {
    				file.mkdir();
    			}
    			
    			file = new File(HISTORY_DIR);
    			if (!file.exists()) {
    				file.mkdir();
    			}
    			
    			file = new File(HISTORY_DIR + contact.getBareJid());
    			if (! file.exists())
    				file.createNewFile();
    			
    			//System.out.println("Logfile: " + file.getCanonicalPath());
    			
    			historyFile = new FileWriter(file, 
    					true);
    					
    		    historyFile.write(System.getProperty("line.separator") 
    		          + DateFormat
                     .getDateTimeInstance(DateFormat.LONG, DateFormat.LONG)
                      .format(new java.util.Date()));
    		}

    		historyFile.write(System.getProperty("line.separator") + text);
    		historyFile.flush();
    	} catch (IOException e) {
    		System.err.println("Couldn't write history to " 
    				+ HISTORY_DIR + contact.getBareJid());
    	}

    }
    
    protected void addToHistory(String text) {
    	original(text);
    	writeToFile(text);
    }
}
