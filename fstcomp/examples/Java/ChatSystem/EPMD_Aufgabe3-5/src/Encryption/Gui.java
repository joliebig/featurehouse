

public class Gui {
	public boolean handleEvent(Event e) {
		if ((e.target == inputField) && (e.id == Event.ACTION_EVENT)) {
			String encodedContent = Encryption.encrptMethod1(Encryption.encrptMethod2((String)e.arg));
			chatClient.send(encodedContent);
			inputField.setText("");
			return true;
		} else if (e.id == Event.WINDOW_DESTROY) {
			if (chatClient != null)
				chatClient.stop();
			setVisible(false);
			System.exit(0);
			return true;
		}
		return super.handleEvent(e);
	}
}