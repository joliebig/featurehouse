


public class Client implements Runnable, ChatComponent {

	public void intialize(String host, int port) throws Exception {
		original(host, port);
		
		System.out.println("GUI START!");
		
		/*if[GUI]*/ 
		new Gui("Chat " + host + ":" + port, this);
		/*end[GUI]*/ 
		
	}
	


}