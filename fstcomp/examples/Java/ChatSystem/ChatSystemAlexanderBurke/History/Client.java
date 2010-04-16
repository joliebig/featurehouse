

public class Client {

	History history;

	public void run() {
		history = new History("Client.txt");
		original();
	}	

	public void fireAddLine(String line) {
		original(line);
		history.onMessageReceived(line);
	}

}