

import java.io.*;
import java.text.*;
import java.util.*;

public class Connection {
	protected void handleIncomingMessage(String name,Object msg) {
		original(name,msg);
		if (msg instanceof TextMessage) {
			archive(name,(TextMessage)msg);
		}
	}
	public synchronized void archive(String name,TextMessage usrMessage){
		BufferedWriter bw = null;
		try{
			bw = new BufferedWriter(new FileWriter("serverLog.log",true));
			TimeZone tz = TimeZone.getTimeZone("EST"); 
         	Date now = new Date();
         	DateFormat df = new SimpleDateFormat ("yyyy.mm.dd hh:mm:ss ");
         	df.setTimeZone(tz);
         	String currentTime = df.format(now);

			bw.write(currentTime + " " + name + " - " + usrMessage.getContent() + "\n");
			bw.newLine();
			bw.flush();
			bw.close();
		} catch(Exception e){}
	}
}