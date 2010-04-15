

public class ServerAuthentication {
	
	public final static String REQ_PASSWORD = "1234";
	
	public boolean authenticate(String username, String password) {
		return password.equals(REQ_PASSWORD);
	}
}
