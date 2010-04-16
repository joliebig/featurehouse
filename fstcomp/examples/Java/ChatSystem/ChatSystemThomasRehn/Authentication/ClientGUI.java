

public class ClientGUI {

	public void initGUI() throws Exception {
		original();
		client.authenticate(clientName, ServerAuthentication.REQ_PASSWORD);
	}
}