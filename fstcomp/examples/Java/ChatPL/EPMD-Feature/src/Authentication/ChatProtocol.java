

public class ChatProtocol {
	private final ServerAuthentication auth = new ServerAuthentication();
	private boolean authorized = false;
	
	protected String handleSpecialAction(String action, String value) {
		String s = original(action, value);
		if (s != null)
			return s;
		if (!authorized && action.equals("auth")) {
			final String[] parts = value.split(USERNAME_PASSWORD_SEP);
			if (parts.length != 2) {
				return returnE(ERROR_STRING);
			}
			if (auth.authenticate(parts[0], parts[1])) {
				authorized = true;
				username = parts[0];
				return returnE(OK_STRING);
			}
		}
		return null;
	}
}
