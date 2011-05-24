


public class Client implements Runnable, ChatComponent {

	public void intialize(String host, int port) throws Exception {
		original(host, port);
		
		System.out.println("Logger START!");
		
		/*if[LOG]*/ 
		new LogWriter(this);
		/*end[LOG]*/
	}
	
}