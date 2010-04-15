


public class Server {
	public void intialize(int port) throws Exception {
		System.out.println("Logger START!");
		
		/*if[LOG]*/ 
		new LogWriter(this);
		/*end[LOG]*/
		
		original(port);
		
		
	}
	

}