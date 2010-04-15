


public class Client implements Runnable, ChatComponent {
	
	public void intialize(String host, int port) throws Exception {
		original(host, port);
		
		/*if[CMD]*/ 
		new CommandLine(this);
		/*end[CMD]*/ 
				
	}
}