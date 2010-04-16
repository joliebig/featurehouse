

public abstract class TransportEncryption {
	private final String name;
	
	public TransportEncryption(String name) {
		this.name = name;
	}
	
	
	
	public abstract String encrypt(String s);
	public abstract String decrypt(String s);



	public String getName() {
		return name;
	}
}
