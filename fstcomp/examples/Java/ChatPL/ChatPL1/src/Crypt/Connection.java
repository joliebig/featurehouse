


public class Connection {
	//Eingehende Nachrichten werden entschl�sselt
	public void handleIncomingMessage(String name, TextMessage msg){
		msg.setContent( fliptext( msg.getContent()) );
		msg.setContent( doConvert( msg.getContent()) );
		original(name,msg);	
		
	}
	//Ausgehende Nachrichten werden entschl�sselt
	public void send(TextMessage msg){
		msg.setContent( fliptext( msg.getContent()) );
		msg.setContent( doConvert( msg.getContent()) );
		original(msg);
	
	}
	
	//String wird vertauscht
	private String fliptext (String text){
		text=new StringBuffer(text).reverse().toString();
		return text;
		
	}
	//ROT 13 
	//Quelle: http://en.literateprograms.org/Rot13_(Java)
	 public String doConvert(String in) {
		String encodedMessage = "";
		for (int i = 0; i < in.length(); i++) {
			char c = in.charAt(i);
			if      (c >= 'a' && c <= 'm') c += 13;
			else if (c >= 'n' && c <= 'z') c -= 13;
			else if (c >= 'A' && c <= 'M') c += 13;
			else if (c >= 'N' && c <= 'Z') c -= 13;
			encodedMessage += c;
		}
		return encodedMessage;
       
    
	}
	
	
}