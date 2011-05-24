


public class Client {

	 public void handleIncomingMessage(TextMessage msg){
	 	 
	 	 if(msg instanceof ColorMessage){
	 	 	 	 fireAddLine("["+((ColorMessage) msg).getColor()+"] " + msg.getContent() + "\n");
	 	 	 	 
	 	 	 	 }
	 	 else{
	 			 fireAddLine(msg.getContent() + "\n");	 
	 			 
	 			  }	 	 	 
	 }

}