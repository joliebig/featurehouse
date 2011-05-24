


public class Client {
	private String passwort="EPMD4EVER";
	
	public void init(){
		original();
		TextMessage auth=new AuthMessage(passwort);
		send (auth);
	}
	
}