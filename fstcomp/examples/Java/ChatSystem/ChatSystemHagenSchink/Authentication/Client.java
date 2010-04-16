

public class Client {

	public void run() {
		send(new ConnectionMessage("password"));
		
		original();
	}
}