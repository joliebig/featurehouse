

import java.io.*;
import java.text.*;
import java.util.*;

public class Client {
	protected void handleIncomingMessage(Object msg) {
		original(msg);
		if (msg instanceof TextMessage) {
			archive((TextMessage)msg);
		}
	}
	public synchronized void archive(TextMessage usrMessage){
		BufferedWriter bw = null;
		try{
			bw = new BufferedWriter(new FileWriter("clientLog.log",true));
			TimeZone tz = TimeZone.getTimeZone("EST"); 
         	Date now = new Date();
         	DateFormat df = new SimpleDateFormat ("yyyy.mm.dd hh:mm:ss ");
         	df.setTimeZone(tz);
         	String currentTime = df.format(now);

			bw.write(currentTime + " " + usrMessage.getContent() + "\n");
			bw.newLine();
			bw.flush();
			bw.close();
		} catch(Exception e){}
	}
}