


import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;


public class LogWriter implements ChatLineListener{

	
	protected ChatComponent chatComp;
	protected BufferedWriter b ;
	
	public LogWriter(ChatComponent chatComp) {

		System.out.println("starting logwriter for " + chatComp.getName());
		
		File f = new File("log");
		f.mkdir();
		
		
		// register listener so that we are informed whenever a new chat message
		// is received (observer pattern)
		chatComp.addLineListener(this);
		this.chatComp = chatComp;
		
				
	}
	
	public void newChatLine(TextMessage msg) {
		// TODO Auto-generated method stub
		try {
			BufferedWriter out = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream("log/Log_" +  chatComp.getName().replaceAll("/","-") + ".txt", true), "ISO-8859-1"));
			String content = msg.getSender()+ ">"+ msg.getSettings() + ">" +  msg.getContent()+"\n";
			out.write(content, 0, content.length());
			out.close();
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        
	}
	

}
