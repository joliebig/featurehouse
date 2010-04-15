

import java.io.File;
import java.io.FileWriter;



public class Client {
	private FileWriter stream;
	private String newline;
	
	public void init(){
		newline=System.getProperty("line.separator");
		try {
            stream = new FileWriter(new File("ClientHistory.txt"), true);
        } catch (Exception e) {
            System.out.println("Die Logdatei konnte die erstellt werden");
        }
        
	}
	
	 public void handleIncomingMessage(TextMessage msg){
	 	original(msg);
	 	try {
	    	stream.write(((TextMessage) msg).getContent());
	        stream.append(newline);
	        stream.flush();
	        } 
	        catch (Exception e) {
	            System.out.println("Logdatei konnte nicht beschrieben werden");
	        }
	 	
	 }
	

}