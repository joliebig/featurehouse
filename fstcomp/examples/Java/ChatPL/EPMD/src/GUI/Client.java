

public class Client {

	private Gui gui;
	
	public void run() {
		gui = new Gui(this);
		original();	
	}
	
	public void fireAddLine(String line) {
		original(line);
		gui.onMessageReceived(line);
	}
}